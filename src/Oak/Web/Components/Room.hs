{-# LANGUAGE TemplateHaskell #-}
module Oak.Web.Components.Room (roomComponent) where

import Web.Spock
import Network.HTTP.Types.Status
import System.Random
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson.TH
import Data.Aeson (encode)
import Control.Applicative
import Control.Monad.Random
import Network.Wai (rawPathInfo)
import Data.Monoid
import Web.Spock.Worker
import Network.Wai.Handler.WebSockets
import Network.Wai
import Network.WebSockets

import Oak.Web.Utils
import Oak.Web.Types
import Oak.Core.Room
import Oak.Core.Booster

-- Objects that front-end works with
data PlayerAPI
  = PlayerAPI
  { plyr_name :: T.Text
  , plyr_picked :: Bool
  }
$(deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''PlayerAPI)

data CardAPI
  = CardAPI
  { card_set :: T.Text
  , card_num :: T.Text
  }
$(deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''CardAPI)

data CardListAPI
  = CardListAPI
  { cdls_draft :: [CardAPI]
  , cdls_pool :: [CardAPI]
  , cdls_picked :: Bool
  }
$(deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''CardListAPI)

data JSONPayload a
  = JSONPayload
  { pyld_name :: String
  , pyld_data :: a
  }
$(deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''JSONPayload)

playerlistToPayload :: [PlayerAPI] -> JSONPayload [PlayerAPI]
playerlistToPayload = JSONPayload "player_list"

cardlistToPayload :: CardListAPI -> JSONPayload CardListAPI
cardlistToPayload = JSONPayload "card_list"

shutdownPayload :: JSONPayload ()
shutdownPayload = JSONPayload "shutdown" ()

unknownPayload :: JSONPayload ()
unknownPayload = JSONPayload "unknown" ()

playerObj :: Player -> PlayerAPI
playerObj = liftA2 PlayerAPI playerName playerPicked

cardObj :: Card -> CardAPI
cardObj = liftA2 CardAPI (T.pack . setToLetters . cardExpansion) cardNumber

cardListObj :: Player -> CardListAPI
cardListObj = liftA3 CardListAPI (map cardObj . playerDraft) (map cardObj . playerPool) (playerPicked)

-- Spock can't differ /foo/bar from /foo/bar/, so we take a raw path and
-- see if there was a trailing slash
directoryHook :: MonadIO m => ActionCtxT ctx m () -> ActionCtxT ctx m ()
directoryHook action = do
  rawPath <- T.decodeUtf8 . rawPathInfo <$> request
  if T.last rawPath /= '/'
  then redirect $ rawPath <> "/"
  else action

roomComponent :: SpockM () UserSession GlobalState ()
roomComponent = do
  let
    roomBaseR   = "room"
    createRoomR = roomBaseR <//> "create" <//> var
    roomR       = roomBaseR <//> var
    nameChangeR = roomR <//> "changename"
    cardPickR   = roomR <//> "pick"       <//> var
    startR      = roomR <//> "start"
    eventsR     = roomR <//> "events"
    joinR       = roomR <//> "join"

    errorHandler = ErrorHandlerIO (\_ _ -> return WorkError)
    workHandler (rid,trooms) = do
      liftIO . atomically . modifyTVar trooms $ (IM.delete rid)
      return WorkComplete

  worker <- newWorker $ WorkerDef (WorkerConfig (1000) (WorkerConcurrentBounded 4)) workHandler errorHandler

  get createRoomR $ \boosters -> do
    liftIO . putStrLn $ "createroom: " ++ show boosters
    uuid <- getUserUUID <$> readSession
    rid <- nextRoomNumber
    trooms <- stateRooms <$> getState
    liftIO . atomically $ do
      troom <- newTVar . (\x -> x { roomHost = uuid }) $ createRoom boosters
      modifyTVar trooms $ (IM.insert rid troom)
      addPlayerSTM uuid $ troom
    addWork (WorkIn 3600) (rid,trooms) worker -- Clean the room after an hour
    redirect (renderRoute roomR (toHashid rid))

  get roomR $ \(ruid :: T.Text) -> directoryHook $ withRoom ruid $ \_ -> do
    file "text/html" "static/room.html"

  post nameChangeR $ \ruid -> withRoom ruid $ \(_, troom) -> do
    name <- T.decodeUtf8 <$> body
    uuid <- getUserUUID <$> readSession
    liftIO . atomically $ do
      modifyTVar troom $ (modifyPlayer uuid (\x -> x { playerName = name }))
      broadcastEvent PlayersUpdate troom

  get cardPickR $ \ruid pick -> withRoom ruid $ \(_, troom) -> do
    uuid <- getUserUUID <$> readSession
    db <- cardDb <$> getState
    room <- liftIO . atomically $ do
      modifyTVar troom (transferCard uuid pick)
      readTVar troom
    when (all playerPicked . roomPlayers $ room) . liftIO $ do
      if (all (null . playerDraft) . roomPlayers $ room)
      then do
        room' <- evalRandTIO $ crackBooster db room
        atomically $ writeTVar troom room'
      else atomically $ modifyTVar troom (rotateCards)
      atomically $ broadcastEvent CardListUpdate troom
    liftIO . atomically $ do
      sendEvent CardListUpdate uuid troom 
      broadcastEvent PlayersUpdate troom

  get startR $ \ruid -> withRoom ruid $ \(_, troom) -> do
    uuid <- getUserUUID <$> readSession
    db <- cardDb <$> getState
    room <- liftIO . readTVarIO $ troom
    if roomHost room /= uuid
    then setStatus forbidden403 >> text "You're not the host!"
    else unless (roomClosed room) $ do
      room' <- liftIO . evalRandTIO $ crackBooster db room
      liftIO . atomically $ do
        writeTVar troom (room' { roomClosed = True })
        broadcastEvent CardListUpdate troom

  get eventsR $ \ruid -> withRoom ruid $ \(_, troom) -> do
    uuid <- getUserUUID <$> readSession
    let
      wsLoop conn = do
        me <- nextEvent uuid troom
        flip (maybe (sendClose conn ("Another connection was opened" :: T.Text))) me $ \e -> do
          room <- atomically $ readTVar troom
          let 
            pl = case e of
              CardListUpdate -> encode . cardlistToPayload
                . cardListObj
                . flip (M.!) uuid
                . roomPlayers
                $ room
              PlayersUpdate -> encode . playerlistToPayload
                . map playerObj 
                . M.elems . roomPlayers
                $ room
              ServerShutdown -> encode shutdownPayload
              _ -> encode unknownPayload
          sendTextData conn pl
          wsLoop conn
      wsApp penconn = do
        conn <- acceptRequest penconn
        atomically $ terminateEventQueue uuid troom
        atomically $ do
          sendEvent CardListUpdate uuid troom
          sendEvent PlayersUpdate uuid troom
        wsLoop conn
      backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
    respondApp $ websocketsOr defaultConnectionOptions wsApp backupApp

  get joinR $ \ruid -> flip (withRoom' ruid) (const $ text "Already joined") $ \(_, troom) -> do
    uuid <- getUserUUID <$> readSession
    liftIO . atomically $ do
      addPlayerSTM uuid $ troom
      broadcastEvent PlayersUpdate troom
    redirect $ renderRoute roomR ruid

withRoom :: T.Text
         -> ((Int, TVar Room) -> SpockActionCtx ctx conn UserSession GlobalState ())
         -> SpockActionCtx ctx conn UserSession GlobalState ()
withRoom rhashid action = withRoom' rhashid (\_ -> file "text/html" "static/join.html") action

withRoom' :: T.Text
         -> ((Int, TVar Room) -> SpockActionCtx ctx conn UserSession GlobalState ())
         -> ((Int, TVar Room) -> SpockActionCtx ctx conn UserSession GlobalState ())
         -> SpockActionCtx ctx conn UserSession GlobalState ()
withRoom' rhashid failedac action = do
  rooms <- liftIO . atomically . readTVar . stateRooms =<< getState
  case fromHashid rhashid of
    Nothing -> do
      setStatus badRequest400
      text "Wrong room ID!"
    Just rid -> do
      case IM.lookup rid rooms of
        Nothing -> do
          setStatus notFound404
          text "Room not found!"
        Just troom -> do
          room <- liftIO . readTVarIO $ troom
          uuid <- getUserUUID <$> readSession
          let isMember = M.member uuid $ roomPlayers room
          if roomClosed room && not isMember
          then do
            setStatus forbidden403
            text "The room is closed"
          else do
            if isMember
            then action (rid, troom)
            else failedac (rid, troom)

nextRoomNumber :: SpockActionCtx ctx conn sess GlobalState Int
nextRoomNumber = do
  t <- stateRooms <$> getState
  rooms <- liftIO . atomically $ readTVar t
  a <- liftIO $ getStdRandom (randomR (0, 0xFFFFFFFF))
  if IM.member a rooms
  then nextRoomNumber
  else return a  

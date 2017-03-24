{-# LANGUAGE TemplateHaskell #-}
module Oak.Web.Components.Room (roomComponent) where

import Web.Spock hiding (head)
import Network.HTTP.Types.Status
import System.Random
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Stream.Infinite as S
import Data.Aeson.TH
import Data.Aeson (encode)
import Control.Applicative
import Network.Wai (rawPathInfo)
import Data.Monoid
import Web.Spock.Worker
import Network.Wai.Handler.WebSockets
import Network.Wai
import Network.WebSockets
import Data.Time.Clock
import Control.Concurrent
import Control.Lens

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
  , card_rarity :: T.Text
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
playerObj = liftA2 PlayerAPI _playerName _playerPicked

cardObj :: Card -> CardAPI
cardObj = liftA3 CardAPI (T.pack . setToLetters . cardExpansion) cardNumber (T.pack . show . cardRarity)

cardListObj :: Player -> CardListAPI
cardListObj = liftA3 CardListAPI (map cardObj . _playerDraft) (map cardObj . _playerPool) (_playerPicked)

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
    addBotR     = roomR <//> "addbot"
    becomeBotR  = roomR <//> "becomebot"
    nameChangeR = roomR <//> "changename"
    cardPickR   = roomR <//> "pick"       <//> var
    startR      = roomR <//> "start"
    eventsR     = roomR <//> "events"
    joinR       = roomR <//> "join"

    errorHandler = ErrorHandlerIO (\_ _ -> return WorkError)
    workHandler () = do
      trooms <- stateRooms <$> getState
      liftIO $ do
        curtime <- getCurrentTime
        diff <- atomically $ do
          rooms <- traverse readTVar <=< readTVar $ trooms
          let
            activeRooms = IM.filter (\x -> diffUTCTime curtime (x ^. roomLastActive) < 900) $ rooms
          modifyTVar trooms $ flip IM.intersection activeRooms
          return (IM.size rooms - IM.size activeRooms)
        unless (diff == 0) . putStrLn $ "Sweep thread: cleaned " ++ show diff ++ " rooms"

      return $ WorkRepeatIn 60

  worker <- newWorker $ WorkerDef (WorkerConfig 1000 WorkerConcurrentUnbounded) workHandler errorHandler
  liftIO $ addWork WorkNow () worker

  get createRoomR $ \boosters -> do
    (roomType :: Maybe T.Text) <- param "type" -- FIXME should be custom type instead of text
    tboxes <- stateBoxes <$> getState
    liftIO . putStrLn $ "createroom: " ++ show boosters
    uuid <- getUserUUID <$> readSession
    rid <- nextRoomNumber
    trooms <- stateRooms <$> getState
    curtime <- liftIO getCurrentTime
    troom <- liftIO $ newTVarIO . (roomHost .~ uuid) $ createRoom boosters curtime
    liftIO . atomically $ do
      modifyTVar trooms $ IM.insert rid troom
      addPlayerSTM uuid troom
    case roomType of
      Just "sealed" -> liftIO . atomically $ do
        crackAllBoosters tboxes troom
        modifyTVar troom (roomClosed .~ True)
      Just "box" -> do
        db <- cardDb <$> getState
        bcycles <- cardCycles <$> getState
        box <- liftIO $ evalRandIO (boxStream db bcycles (head boosters)) -- FIXME: Taking 36 boosters is not an okay solution
        liftIO . atomically $ do
          modifyTVar troom (roomPlayer uuid . playerPool .~ concat (S.take 36 box))
          modifyTVar troom (roomClosed .~ True)
      _ -> return ()

    redirect (renderRoute roomR (toHashid rid))

  get roomR $ \(ruid :: T.Text) -> directoryHook $ withRoom ruid $ \_ -> do
    file "text/html" "static/room.html"

  get addBotR $ \(ruid :: T.Text) -> withRoom ruid $ \(_, troom) -> do
    room <- liftIO . atomically $ readTVar troom
    uuid <- getUserUUID <$> readSession
    if uuid == room ^. roomHost
    then liftIO $ do
      addBot troom
      atomically $ broadcastEvent PlayersUpdate troom
    else text "You're not the host"

  get becomeBotR $ \(ruid :: T.Text) -> withRoom ruid $ \(_, troom) -> do
    uuid <- getUserUUID <$> readSession
    liftIO . atomically $ modifyTVar troom (roomPlayer uuid . playerBot .~ True)

  post nameChangeR $ \ruid -> withRoom ruid $ \(_, troom) -> do
    name <- T.decodeUtf8 <$> body
    uuid <- getUserUUID <$> readSession
    liftIO . atomically $ do
      modifyTVar troom $ (roomPlayer uuid . playerName .~ name)
      broadcastEvent PlayersUpdate troom

  get cardPickR $ \ruid pick -> withRoom ruid $ \(_, troom) -> do
    uuid <- getUserUUID <$> readSession
    db <- cardDb <$> getState
    tboxes <- stateBoxes <$> getState
    room <- liftIO . atomically $ do
      modifyTVar troom (transferCard uuid pick)
      readTVar troom
    when (all _playerPicked . _roomPlayers $ room) . liftIO . atomically $ do
      if (all (null . _playerDraft) . _roomPlayers $ room)
      then crackBooster tboxes troom
      else modifyTVar troom (rotateCards)
      botsPick troom
      broadcastEvent CardListUpdate troom
    liftIO . atomically $ do
      sendEvent CardListUpdate uuid troom 
      broadcastEvent PlayersUpdate troom

  get startR $ \ruid -> withRoom ruid $ \(_, troom) -> do
    uuid <- getUserUUID <$> readSession
    db <- cardDb <$> getState
    tboxes <- stateBoxes <$> getState
    room <- liftIO . readTVarIO $ troom
    if room ^. roomHost /= uuid
    then setStatus forbidden403 >> text "You're not the host!"
    else unless (room ^. roomClosed) $ liftIO . atomically $ do
        crackBooster tboxes troom
        modifyTVar troom (roomClosed .~ True)
        botsPick troom
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
                . _roomPlayers
                $ room
              PlayersUpdate -> encode . playerlistToPayload
                . map playerObj 
                . M.elems . _roomPlayers
                $ room
              ServerShutdown -> encode shutdownPayload
              _ -> encode unknownPayload
          sendPing conn ("" :: T.Text)
          sendTextData conn pl
          wsLoop conn
      wsApp penconn = do
        conn <- acceptRequest penconn
        forkPingThread conn 60
        atomically $ terminateEventQueue uuid troom
        atomically $ do
          void $ initEventQueue uuid troom
          sendEvent CardListUpdate uuid troom
          sendEvent PlayersUpdate uuid troom
        void . forkIO $ wsLoop conn
        forever . void $ receiveDataMessage conn
      backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
      customPong = do
        curtime <- getCurrentTime
        atomically $ modifyTVar troom (roomLastActive .~ curtime)
    respondApp $ websocketsOr (defaultConnectionOptions { connectionOnPong = customPong }) wsApp backupApp

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
          let isMember = M.member uuid $ room ^. roomPlayers
          if room ^. roomClosed && not isMember
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

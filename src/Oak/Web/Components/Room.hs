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
import Control.Applicative
import Control.Monad.Random
import Network.Wai (rawPathInfo)
import Data.Monoid

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
    playerListR = roomR <//> "playerlist"
    cardListR   = roomR <//> "cardlist"
    startR      = roomR <//> "start"
    eventsR     = roomR <//> "events"

  get createRoomR $ \boosters -> do
    uuid <- getUserUUID <$> readSession
    rid <- nextRoomNumber
    trooms <- stateRooms <$> getState
    liftIO . atomically $ do
      troom <- newTVar . (\x -> x { roomHost = uuid }) $ createRoom boosters
      modifyTVar trooms $ (IM.insert rid troom)
      addPlayerSTM uuid $ troom
    redirect (renderRoute roomR (toHashid rid))

  get roomR $ \(ruid :: T.Text) -> directoryHook $ withRoom ruid $ \_ -> do
    file "text/html" "static/room.html"

  get playerListR $ \(ruid :: T.Text) -> withRoom ruid $ \(_, troom) -> do
    room <- liftIO . readTVarIO $ troom
    json . map playerObj 
         . M.elems . roomPlayers
         $ room

  get cardListR $ \(ruid :: T.Text) -> withRoom ruid $ \(_, troom) -> do
    uuid <- getUserUUID <$> readSession
    room <- liftIO . readTVarIO $ troom
    json . cardListObj
         . flip (M.!) uuid
         . roomPlayers
         $ room

  post nameChangeR $ \ruid -> withRoom ruid $ \(_, troom) -> do
    name <- T.decodeUtf8 <$> body
    uuid <- getUserUUID <$> readSession
    liftIO . atomically $ do
      modifyTVar troom $ (modifyPlayer uuid (\x -> x { playerName = name }))
      room <- readTVar troom
      broadcastEvent PlayersUpdate room

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
      atomically $ broadcastEvent CardListUpdate room
    liftIO . atomically $ do
      sendEvent CardListUpdate uuid troom 
      broadcastEventTVar PlayersUpdate troom

  get startR $ \ruid -> withRoom ruid $ \(_, troom) -> do
    uuid <- getUserUUID <$> readSession
    db <- cardDb <$> getState
    room <- liftIO . readTVarIO $ troom
    if roomHost room /= uuid
    then setStatus forbidden403 >> text "You're not the host!"
    else unless (roomClosed room) $ do
      room' <- liftIO . evalRandTIO $ crackBooster db room
      liftIO . atomically $ writeTVar troom (room' { roomClosed = True })
      liftIO . atomically $ broadcastEvent CardListUpdate room

  get eventsR $ \ruid -> withRoom ruid $ \(_, troom) -> do
    uuid <- getUserUUID <$> readSession
    e <- liftIO . atomically $ readTVar troom >>= nextEvent uuid
    text $ T.pack $ show e

withRoom :: T.Text
         -> ((Int, TVar Room) -> SpockActionCtx ctx conn UserSession GlobalState ())
         -> SpockActionCtx ctx conn UserSession GlobalState ()
withRoom rhashid action = do
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
            unless isMember . liftIO . atomically $ do
              addPlayerSTM uuid $ troom
              broadcastEventTVar PlayersUpdate troom
            action (rid, troom)

nextRoomNumber :: SpockActionCtx ctx conn sess GlobalState Int
nextRoomNumber = do
  t <- stateRooms <$> getState
  rooms <- liftIO . atomically $ readTVar t
  a <- liftIO $ getStdRandom (randomR (0, 0xFFFFFFFF))
  if IM.member a rooms
  then nextRoomNumber
  else return a  

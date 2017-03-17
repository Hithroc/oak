{-# LANGUAGE TemplateHaskell #-}
module Oak.Core.Room where
import Data.UUID (UUID, nil)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Maybe
import Control.Monad.Random
import Control.Monad.State
import Control.Concurrent.STM
import Data.SafeCopy
import Data.UUID.SafeCopy ()
import Data.Serialize (Get)
import Data.Time.Clock
import qualified Data.Stream.Infinite as S
import Control.Lens

import Oak.Core.Booster

data Event
  = PlayersUpdate
  | CardListUpdate
  | ServerShutdown
  | TerminateListener
  deriving Show

data Direction = DLeft | DRight
  deriving Show
deriveSafeCopy 1 'base ''Direction

changeDirection :: Direction -> Direction
changeDirection DLeft = DRight
changeDirection DRight = DLeft

data PlayerEventQueue
  = PlayerEventQueue
  { _playerEvents :: TQueue Event
  }
makeLenses ''PlayerEventQueue

newtype MPlayerEventQueue = MPlayerEventQueue (Maybe PlayerEventQueue)

mPlayerEventQueueIso :: Iso MPlayerEventQueue (Maybe PlayerEventQueue)
mPlayerEventQueueIso = iso (\(MPlayerEventQueue q) -> q) (\q -> MPlayerEventQueue q)

instance Show MPlayerEventQueue where
  show _ = "EventQueue"

instance SafeCopy (MPlayerEventQueue) where
  version = 1
  putCopy (MPlayerEventQueue Nothing) = contain $ safePut ()
  putCopy (MPlayerEventQueue _) = putCopy (MPlayerEventQueue Nothing)
  getCopy = contain $ const (MPlayerEventQueue Nothing) <$> (safeGet :: Get ())

data Player
  = Player
  { _playerName :: Text
  , _playerDraft :: [Card]
  , _playerPool :: [Card]
  , _playerPicked :: Bool
  , _mPlayerEventQueue :: MPlayerEventQueue
  }
  deriving Show
deriveSafeCopy 1 'base ''Player
makeLenses ''Player

playerEventQueue :: Lens' Player (Maybe PlayerEventQueue)
playerEventQueue = mPlayerEventQueue . mPlayerEventQueueIso

defaultPlayer :: Player
defaultPlayer
  = Player
  { _playerName = "Trixie Lulamoon"
  , _playerDraft = []
  , _playerPool = []
  , _playerPicked = False
  , _playerEventQueue = (MPlayerEventQueue Nothing)
  }

data Room
  = Room
  { _roomPlayers :: Map UUID Player
  , _roomBoosters :: [BoosterType]
  , _roomClosed :: Bool 
  , _roomHost :: UUID
  , _roomDirection :: Direction
  , _roomLastActive :: UTCTime
  }
  deriving Show
deriveSafeCopy 1 'base ''Room
makeLenses ''Room

roomPlayer :: UUID -> Prism' Room Player
roomPlayer uuid = roomPlayers . at uuid . _Just

createRoom :: [BoosterType] -> UTCTime -> Room
createRoom btype time
  = Room 
  { _roomPlayers = M.empty
  , _roomBoosters = btype
  , _roomClosed = False
  , _roomHost = nil
  , _roomDirection = DLeft
  , _roomLastActive = time
  }

addPlayer :: UUID -> Room -> Room
addPlayer uuid room = room & roomPlayers . at uuid ?~ defaultPlayer

initEventQueue :: UUID -> TVar Room -> STM PlayerEventQueue
initEventQueue uuid troom = do
  room <- readTVar troom
  let queue = roomPlayer uuid . playerEventQueue
  case room ^? queue . _Just of
    Just eq -> return eq
    Nothing -> do
      eq <- PlayerEventQueue <$> newTQueue
      modifyTVar troom (queue ?~ eq)
      --modifyTVar troom (modifyPlayer uuid (\p -> p { playerEventQueue = MPlayerEventQueue $ Just eq }))
      return eq

terminateEventQueue :: UUID -> TVar Room -> STM ()
terminateEventQueue uuid troom = do
  sendEvent TerminateListener uuid troom
  modifyTVar troom (roomPlayer uuid . playerEventQueue .~ Nothing)

addPlayerSTM :: UUID -> TVar Room -> STM ()
addPlayerSTM uuid = flip modifyTVar (addPlayer uuid)

addPlayerIO :: UUID -> TVar Room -> IO ()
addPlayerIO uuid = atomically . addPlayerSTM uuid

eventPrism :: _
eventPrism target = roomPlayers . target . playerEventQueue . _Just . playerEvents %%~ flip writeTQueue e

broadcastEvent :: Event -> TVar Room -> STM ()
broadcastEvent e troom = void $ readTVar troom >>= eventPrism each
{-broadcastEvent e troom = readTVar troom >>= sequence_
  . map (flip writeTQueue e . playerEvents <=< flip initEventQueue troom)
  . M.keys
  . roomPlayers-}

sendEvent :: Event -> UUID -> TVar Room -> STM ()
sendEvent e uuid = void $ readTVar troom >>= eventPrism (at uuid . _Just)

nextEvent :: UUID -> TVar Room -> IO (Maybe Event)
nextEvent uuid troom = do
  eq <- atomically $ initEventQueue uuid troom
  let tq = eq ^. playerEvents
  e <- atomically $ readTQueue $ tq
  case e of
    TerminateListener -> return Nothing
    _ -> return $ Just e

crackBooster :: TVar (M.Map BoosterType (S.Stream [Card])) -> TVar Room -> STM ()
crackBooster tboxes troom = do
  room <- readTVar troom
  boxes <- readTVar tboxes
  case roomBoosters room of
    [] -> return ()
    (btype:bs) -> case M.lookup btype boxes of
      Nothing -> return ()
      Just boxStream -> do
        (pl', boxStream') <- runStateT (traverse givePlayer $ roomPlayers room) boxStream
        modifyTVar tboxes (M.insert btype boxStream')
        modifyTVar troom (\r -> r { roomPlayers = pl', roomBoosters = bs, roomDirection = changeDirection (roomDirection room) })
  where
    givePlayer :: MonadState (S.Stream [Card]) m => Player -> m Player
    givePlayer p = do
      booster <- head . S.take 1 <$> get
      modify S.tail
      return $ p { playerDraft = playerDraft p ++ booster, playerPicked = False }

pop :: Int -> [a] -> (Maybe a, [a])
pop _ [] = (Nothing, [])
pop i l
  | i < 0 && i >= length l = (Nothing, l)
  | otherwise = (Just . head $ b, a ++ tail b)
    where (a,b) = splitAt i l

transferCard :: UUID -> Int -> Room -> Room
transferCard uuid index room = modifyPlayer uuid (\p -> if playerPicked p then p else f p) room
  where
    f p = p { playerPicked = True, playerDraft = snd . popped $ p, playerPool = maybe id (:) (fst . popped $ p) (playerPool p) }
    popped p = pop (index `mod` length (playerDraft p)) $ playerDraft p

transferAllCards :: Room -> Room
transferAllCards r = r { roomPlayers = fmap (\p -> p { playerPool = playerDraft p, playerDraft = playerPool p }) $ roomPlayers r }

crackAllBoosters :: TVar (M.Map BoosterType (S.Stream [Card])) -> TVar Room -> STM ()
crackAllBoosters tboxes troom = do
  room <- readTVar troom
  if null $ roomBoosters room
  then modifyTVar troom transferAllCards
  else do
    crackBooster tboxes troom
    crackAllBoosters tboxes troom

shift :: Direction -> [a] -> [a]
shift _ [] = []
shift DLeft (x:xs) = xs ++ [x]
shift DRight xs = last xs:init xs

rotateCards :: Room -> Room
rotateCards room = room { roomPlayers = pl' }
  where
    pl = roomPlayers room
    dir = roomDirection room
    drafts = shift dir . map (playerDraft . snd) $ M.toList pl
    pl' = M.fromList . zipWith (\draft (uuid,player) -> (uuid, player { playerDraft = draft, playerPicked = False } )) drafts . M.toList $ pl

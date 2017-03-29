{-# LANGUAGE TemplateHaskell #-}
module Oak.Core.Room where
import Data.UUID (UUID, nil)
import Data.UUID.V4
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
import Data.List

import Oak.Core.Booster
import Oak.Core.Room.Bot

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

newtype PlayerEventQueue
  = PlayerEventQueue
  { _playerEvents :: TQueue Event
  }
makeLenses ''PlayerEventQueue

newtype MPlayerEventQueue = MPlayerEventQueue (Maybe PlayerEventQueue)

mPlayerEventQueueIso :: Iso' MPlayerEventQueue (Maybe PlayerEventQueue)
mPlayerEventQueueIso = iso (\(MPlayerEventQueue q) -> q) MPlayerEventQueue

instance Show MPlayerEventQueue where
  show _ = "EventQueue"

instance SafeCopy MPlayerEventQueue where
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
  , _playerBot :: Bool
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
  , _playerBot = False
  , _mPlayerEventQueue = MPlayerEventQueue Nothing
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

roomPlayer :: UUID -> Traversal' Room Player
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

addBot :: TVar Room -> IO ()
addBot troom = do
  uuid <- nextRandom
  atomically $ modifyTVar troom (roomPlayers . at uuid ?~ defaultPlayer { _playerName = "Sweetie Belle", _playerBot = True })

initEventQueue :: UUID -> TVar Room -> STM PlayerEventQueue
initEventQueue uuid troom = do
  room <- readTVar troom
  let
    queue :: Traversal' Room (Maybe PlayerEventQueue)
    queue = roomPlayer uuid . playerEventQueue
  case room ^? queue . _Just of
    Just eq -> return eq
    Nothing -> do
      eq <- PlayerEventQueue <$> newTQueue
      modifyTVar troom (queue ?~ eq)
      return eq

terminateEventQueue :: UUID -> TVar Room -> STM ()
terminateEventQueue uuid troom = do
  sendEvent TerminateListener uuid troom
  modifyTVar troom (roomPlayer uuid . playerEventQueue .~ Nothing)

addPlayerSTM :: UUID -> TVar Room -> STM ()
addPlayerSTM uuid = flip modifyTVar (addPlayer uuid)

addPlayerIO :: UUID -> TVar Room -> IO ()
addPlayerIO uuid = atomically . addPlayerSTM uuid

eventPrism :: Event -> ((Player -> STM Player) -> Map UUID Player -> STM (Map UUID Player)) -> Room -> STM Room
eventPrism e target = roomPlayers . target . playerEventQueue . _Just . playerEvents %%~ \x -> writeTQueue x e >> return x

broadcastEvent :: Event -> TVar Room -> STM ()
broadcastEvent e troom = void $ readTVar troom >>= eventPrism e each

sendEvent :: Event -> UUID -> TVar Room -> STM ()
sendEvent e uuid troom = void $ readTVar troom >>= eventPrism e (at uuid . _Just)

nextEvent :: UUID -> TVar Room -> IO (Maybe Event)
nextEvent uuid troom = do
  eq <- atomically $ initEventQueue uuid troom
  let tq = eq ^. playerEvents
  e <- atomically . readTQueue $ tq
  case e of
    TerminateListener -> return Nothing
    _ -> return $ Just e

botsPick :: TVar Room -> STM ()
botsPick troom = do
  room <- readTVar troom
  let bots = M.filter _playerBot $ room ^. roomPlayers
      scorePick pl = do
        (card, _) <- listToMaybe $ sortedScore (_playerPool pl) (_playerDraft pl)
        card `elemIndex` _playerDraft pl
      tfs = fmap (\(uuid, pl) -> transferCard uuid $ fromMaybe 0 (scorePick pl)) . M.toList $ bots
  modifyTVar troom (foldr (.) id tfs)

crackBooster :: TVar (M.Map BoosterType (S.Stream [Card])) -> TVar Room -> STM ()
crackBooster tboxes troom = do
  room <- readTVar troom
  boxes <- readTVar tboxes
  case room ^. roomBoosters of
    [] -> return ()
    (btype:bs) -> case M.lookup btype boxes of
      Nothing -> return ()
      Just stream -> do
        (pl', stream') <- runStateT (traverse givePlayer $ room ^. roomPlayers) stream
        modifyTVar tboxes (M.insert btype stream')
        modifyTVar troom (\r -> r { _roomPlayers = pl', _roomBoosters = bs })
        modifyTVar troom (roomDirection %~ changeDirection)
  where
    givePlayer :: MonadState (S.Stream [Card]) m => Player -> m Player
    givePlayer p = do
      booster <- head . S.take 1 <$> get
      modify S.tail
      return $ p { _playerDraft = _playerDraft p ++ booster, _playerPicked = False }

pop :: Int -> [a] -> (Maybe a, [a])
pop _ [] = (Nothing, [])
pop i l
  | i < 0 && i >= length l = (Nothing, l)
  | otherwise = (Just . head $ b, a ++ tail b)
    where (a,b) = splitAt i l

transferCard :: UUID -> Int -> Room -> Room
transferCard uuid cid = roomPlayer uuid %~ f
  where
    f p = if _playerPicked p then p else p { _playerPicked = True, _playerDraft = snd . popped $ p, _playerPool = maybe id (:) (fst . popped $ p) (p ^. playerPool) }
    popped p = pop cid $ p ^. playerDraft

transferAllCards :: Room -> Room
transferAllCards = roomPlayers . traverse %~ \p -> p { _playerPool = _playerDraft p, _playerDraft = _playerPool p }

crackAllBoosters :: TVar (M.Map BoosterType (S.Stream [Card])) -> TVar Room -> STM ()
crackAllBoosters tboxes troom = do
  room <- readTVar troom
  if null $ room ^. roomBoosters
  then modifyTVar troom transferAllCards
  else do
    crackBooster tboxes troom
    crackAllBoosters tboxes troom

shift :: Direction -> [a] -> [a]
shift _ [] = []
shift DLeft (x:xs) = xs ++ [x]
shift DRight xs = last xs:init xs

rotateCards :: Room -> Room
rotateCards room = room & roomPlayers .~ pl'
  where
    pl = room ^. roomPlayers
    dir = room ^. roomDirection
    drafts = shift dir . map (_playerDraft . snd) $ M.toList pl
    pl' = M.fromList . zipWith (\draft (uuid,player) -> (uuid, player { _playerDraft = draft, _playerPicked = False } )) drafts . M.toList $ pl

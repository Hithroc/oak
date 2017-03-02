module Oak.Core.Room where
import Data.UUID (UUID, nil)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Maybe
import Control.Monad.Random.Strict
import Control.Concurrent.STM

import Oak.Core.Booster

data Event
  = PlayersUpdate
  | CardListUpdate
  deriving Show

data Player
  = Player
  { playerName :: Text
  , playerDraft :: [Card]
  , playerPool :: [Card]
  , playerPicked :: Bool
  , playerEvents :: TQueue Event
  }

defaultPlayer :: TQueue Event -> Player
defaultPlayer queue
  = Player
  { playerName = "Trixie Lulamoon"
  , playerDraft = []
  , playerPool = []
  , playerPicked = False
  , playerEvents = queue
  }

data Room
  = Room
  { roomPlayers :: Map UUID Player
  , roomBoosters :: [BoosterType]
  , roomClosed :: Bool 
  , roomHost :: UUID
  }

createRoom :: [BoosterType] -> Room
createRoom btype
  = Room 
  { roomPlayers = M.empty
  , roomBoosters = btype
  , roomClosed = False
  , roomHost = nil
  }

addPlayer :: UUID -> TQueue Event -> Room -> Room
addPlayer uuid queue room = room { roomPlayers = M.insert uuid (defaultPlayer queue) (roomPlayers room) }

addPlayerSTM :: UUID -> TVar Room -> STM ()
addPlayerSTM uuid room = do
  queue <- newTQueue
  modifyTVar room (addPlayer uuid queue)

addPlayerIO :: UUID -> TVar Room -> IO ()
addPlayerIO uuid = atomically . addPlayerSTM uuid

modifyPlayer :: UUID -> (Player -> Player) -> Room -> Room
modifyPlayer uuid f room = room { roomPlayers = M.adjust f uuid (roomPlayers room) }


broadcastEvent :: Event -> Room -> STM ()
broadcastEvent e
  = sequence_
  . map (flip writeTQueue e . playerEvents)
  . M.elems 
  . roomPlayers

broadcastEventTVar :: Event -> TVar Room -> STM ()
broadcastEventTVar e troom = do
  room <- readTVar troom
  broadcastEvent e room

nextEvent :: UUID -> Room -> STM Event
nextEvent uuid room = readTQueue (playerEvents $ (roomPlayers room) M.! uuid )

sendEvent :: Event -> UUID -> TVar Room -> STM ()
sendEvent e uuid = sendEvent' e uuid <=< readTVar

sendEvent' :: Event -> UUID -> Room -> STM ()
sendEvent' e uuid room = maybe (return ()) (flip writeTQueue e) tq
  where tq = playerEvents <$> M.lookup uuid (roomPlayers room)

crackBooster :: MonadRandom m => CardDatabase -> Room -> m Room
crackBooster db room = if null (roomBoosters room) then return room else do
  pl' <- sequence $ M.map (givePlayer (head . roomBoosters $ room)) (roomPlayers room)
  return $ room { roomPlayers = pl', roomBoosters = tail (roomBoosters room) }
  where
    givePlayer :: MonadRandom m => BoosterType -> Player -> m Player
    givePlayer s p = do
      b <- generateBooster s db
      return $ p { playerDraft = playerDraft p ++ (catMaybes b) }

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

shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

rotateCards :: Room -> Room
rotateCards room = room { roomPlayers = pl' }
  where
    pl = roomPlayers room
    drafts = shift . map (playerDraft . snd) $ M.toList pl
    pl' = M.fromList . zipWith (\draft (uuid,player) -> (uuid, player { playerDraft = draft, playerPicked = False } )) drafts . M.toList $ pl

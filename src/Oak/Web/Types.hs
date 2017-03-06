module Oak.Web.Types where

import qualified Data.IntMap as IM
import Control.Concurrent.STM (TVar, TMVar)
import Data.UUID (UUID)

import Oak.Core.Room
import Oak.Core.Booster

type Rooms = TVar (IM.IntMap (TVar Room))

data GlobalState
  = GlobalState
  { roomCounter :: TVar Int
  , stateRooms :: Rooms
  , cardDb :: CardDatabase
  , eventsMVar :: TMVar UUID
  }
data UserSession
  = UserSession
  { getUserUUID :: UUID
  }
  deriving Show

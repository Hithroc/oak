{-# LANGUAGE TemplateHaskell #-}
module Oak.Web.Types where

import Data.IntMap (IntMap)
import Control.Concurrent.STM (TVar)
import Data.UUID (UUID)
import Data.SafeCopy
import Data.UUID.SafeCopy ()
import Data.Map (Map)
import Data.Stream.Infinite (Stream)

import Oak.Core.Room
import Oak.Core.Booster

type Rooms = TVar (IntMap (TVar Room))

data GlobalState
  = GlobalState
  { stateRooms :: Rooms
  , cardDb :: CardDatabase
  , cardCycles :: BoosterCycles
  , stateBoxes :: TVar (Map BoosterType (Stream [Card]))
  }
data UserSession
  = UserSession
  { getUserUUID :: UUID
  }
  deriving Show
deriveSafeCopy 1 'base ''UserSession

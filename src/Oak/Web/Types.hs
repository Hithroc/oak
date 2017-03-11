{-# LANGUAGE TemplateHaskell #-}
module Oak.Web.Types where

import qualified Data.IntMap as IM
import Control.Concurrent.STM (TVar)
import Data.UUID (UUID)
import Data.SafeCopy
import Data.UUID.SafeCopy ()

import Oak.Core.Room
import Oak.Core.Booster

type Rooms = TVar (IM.IntMap (TVar Room))

data GlobalState
  = GlobalState
  { stateRooms :: Rooms
  , cardDb :: CardDatabase
  }
data UserSession
  = UserSession
  { getUserUUID :: UUID
  }
  deriving Show
deriveSafeCopy 1 'base ''UserSession

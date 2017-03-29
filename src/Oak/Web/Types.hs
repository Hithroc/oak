{-# LANGUAGE TemplateHaskell #-}
module Oak.Web.Types where

import Data.IntMap (IntMap)
import Control.Concurrent.STM (TVar)
import Data.UUID (UUID)
import Data.SafeCopy
import Data.UUID.SafeCopy ()
import Data.Map (Map)
import Data.Stream.Infinite (Stream)
import Data.Text (Text)

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
data UserSessionOld
  = UserSessionOld
  { oldGetUserUUID :: UUID
  }
  deriving Show
deriveSafeCopy 1 'base ''UserSessionOld

data UserSession
  = UserSession
  { getUserUUID :: UUID
  , userName :: Text
  }
  deriving Show

instance Migrate UserSession where
  type MigrateFrom UserSession = UserSessionOld
  migrate u = UserSession (oldGetUserUUID u) "Trixie Lulamoon"

deriveSafeCopy 2 'extension ''UserSession

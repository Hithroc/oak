{-# LANGUAGE TemplateHaskell, ConstraintKinds #-}
module Oak.Web.SessionStore where

import Web.Spock.Config
import Web.Spock.Internal.SessionManager
import Web.Spock.Internal.SessionVault
import Oak.Web.Types
import Data.Acid
import Data.SafeCopy
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Typeable
import Data.Time.Clock
import Data.Text (Text)


data SessWrapper
  = SessWrapper
  { sessw_id :: SessionId
  , sessw_csrfToken :: Text
  , sessw_validUntil :: UTCTime
  , sessw_data :: UserSession
  }
deriveSafeCopy 1 'base ''SessWrapper

wrapSession :: Session conn UserSession st -> SessWrapper
wrapSession sess
  = SessWrapper
  { sessw_id = sess_id sess
  , sessw_csrfToken = sess_csrfToken sess
  , sessw_validUntil = sess_validUntil sess
  , sessw_data = sess_data sess
  }

unwrapSession :: SessWrapper -> Session conn UserSession st
unwrapSession sess = Session
  { sess_id = sessw_id sess
  , sess_csrfToken = sessw_csrfToken sess
  , sess_validUntil = sessw_validUntil sess
  , sess_data = sessw_data sess
  }

newtype SessionMap = SessionMap (M.Map SessionId SessWrapper)
deriveSafeCopy 1 'base ''SessionMap

runSessionMap :: SessionMap -> M.Map SessionId SessWrapper
runSessionMap (SessionMap m) = m

runTx :: IO () -> IO ()
runTx = id

acidLoadSession :: SessionId -> Query SessionMap (Maybe SessWrapper)
acidLoadSession sessid = do
 SessionMap m <- ask 
 return (sessid `M.lookup` m)

acidDeleteSession :: SessionId -> Update SessionMap ()
acidDeleteSession sessid = do
  SessionMap m <- get
  put (SessionMap $ M.delete sessid m)

acidStoreSession :: SessWrapper -> Update SessionMap ()
acidStoreSession sess = do
  SessionMap m <- get
  put (SessionMap $ M.insert (sessw_id sess) sess m)

acidToList :: Query SessionMap [SessWrapper]
acidToList = M.elems . runSessionMap <$> ask

acidReadMap :: Query SessionMap (M.Map SessionId SessWrapper)
acidReadMap = runSessionMap <$> ask

acidWriteMap :: M.Map SessionId SessWrapper -> Update SessionMap ()
acidWriteMap = put . SessionMap

$(makeAcidic ''SessionMap ['acidLoadSession, 'acidDeleteSession, 'acidStoreSession, 'acidToList, 'acidReadMap, 'acidWriteMap])

oakLoadSession :: AcidState SessionMap-> SessionId -> IO (Maybe (Session conn UserSession st))
oakLoadSession acid sessid = fmap (fmap unwrapSession) $ query acid (AcidLoadSession sessid)

oakDeleteSession :: AcidState SessionMap -> SessionId -> IO ()
oakDeleteSession acid sessid = update acid (AcidDeleteSession sessid)

oakStoreSession :: AcidState SessionMap -> (Session conn UserSession st) -> IO ()
oakStoreSession acid sess = update acid (AcidStoreSession (wrapSession sess))

oakToList :: AcidState SessionMap -> IO [Session conn UserSession st]
oakToList acid = fmap (map unwrapSession) $ query acid AcidToList

oakFilterSessions :: AcidState SessionMap -> (Session conn UserSession st -> Bool) -> IO ()
oakFilterSessions acid p = do
  sesss <- query acid AcidReadMap
  update acid (AcidWriteMap . fmap wrapSession . M.filter p . fmap unwrapSession $ sesss)

oakMapSessions :: AcidState SessionMap -> (Session conn UserSession st -> IO (Session conn UserSession st)) -> IO ()
oakMapSessions acid f = do
  sesss <- query acid AcidReadMap
  sesss' <- traverse (f . unwrapSession) sesss
  update acid (AcidWriteMap . fmap wrapSession $ sesss')

oakSessionStore :: IO (SessionStore (Session conn UserSession st) IO)
oakSessionStore = do
  acid <- openLocalStateFrom "acid_sessions" (SessionMap $ M.empty)
  return $ SessionStore
    { ss_runTx = id
    , ss_loadSession = oakLoadSession acid
    , ss_deleteSession = oakDeleteSession acid
    , ss_storeSession = oakStoreSession acid
    , ss_toList = oakToList acid
    , ss_filterSessions = oakFilterSessions acid
    , ss_mapSessions = oakMapSessions acid
    }

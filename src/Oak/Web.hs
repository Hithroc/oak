module Oak.Web (runApp) where

import Web.Spock
import Web.Spock.Config
import Network.HTTP.Types.Status
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM
import qualified Data.UUID as UUID
import Data.UUID.V4
import qualified Data.IntMap as IM
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Wai.Middleware.Static
import Data.String (fromString)
import qualified Data.ByteString as BS
import Data.SafeCopy
import qualified Data.Serialize as S
import Oak.Core.Room (Room)

import Oak.Web.Types
import Oak.Web.Components
import Oak.Core.Booster
import Oak.Config

customError :: MonadIO m => Status -> ActionCtxT ctx m ()
customError s = do
  setStatus s
  text $ T.pack (show $ statusCode s) 
      <> " - "
      <> (T.decodeUtf8 $ statusMessage s)

sessionHook :: ActionCtxT ctx (WebStateM conn UserSession st) ()
sessionHook = do
  setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  setHeader "Pragma" "no-cache"
  setHeader "Expires" "0"
  uuid <- getUserUUID <$> readSession
  when (UUID.null uuid) $ do
    nuuid <- liftIO $ nextRandom
    modifySession $ (\x -> x { getUserUUID = nuuid })

serializeRooms :: Rooms -> STM (S.Put)
serializeRooms trooms = do
  rooms <- readTVar trooms
  srooms <- sequence $ fmap readTVar rooms
  return $ safePut srooms

deserializeRooms :: BS.ByteString -> STM (Either String Rooms)
deserializeRooms str = case S.runGet (safeGet :: S.Get (IM.IntMap Room)) str of
  Left e -> return $ Left e
  Right rooms -> fmap Right . newTVar <=< sequence . fmap newTVar $ rooms

runApp :: Config -> CardDatabase -> IO ()
runApp cfg db = do
  let settings = setPort (cfg_port cfg)
               . setHost (fromString $ cfg_host cfg)
               . setTimeout 3600
               $ defaultSettings
  trooms <- atomically $ newTVar (IM.empty)
  spockCfg <- defaultSpockCfg (UserSession UUID.nil) PCNoDatabase (GlobalState trooms db)
  spockapp <- spockAsApp (spock (spockCfg { spc_errorHandler = customError }) app)
  runSettings (settings) (spockapp)


app :: SpockM () UserSession GlobalState ()
app = do
  middleware . staticPolicy $ only
    [("app.js", "frontend/dist/app.js")
    ,("home.js", "frontend/dist/home.js")
    ] <|> addBase "static"
  prehook sessionHook $ do
    get root $ file "text/html" "static/index.html"
    roomComponent
    get ("debug" <//> "exception") $ error "[](/rdwut)"
    get ("debug" <//> "serialize") $ do
      trooms <- stateRooms <$> getState
      p <- liftIO . atomically $ serializeRooms trooms
      liftIO $ BS.writeFile "rooms" (S.runPut p)
    get ("debug" <//> "deserialize") $ do
      str <- liftIO $ BS.readFile "rooms"
      trooms <- stateRooms <$> getState
      r <- liftIO . atomically $ do
        t <- deserializeRooms str
        case t of
          Left e -> return $ fromString e
          Right t' -> do
            t'' <- readTVar t'
            writeTVar trooms t''
            return "Success"
      text r
    get ("debug" <//> "session") $ do
      sess <- readSession
      sessid <- getSessionId
      text . T.pack $ show sess <> show sessid
    get ("debug" <//> "status" <//> var) $ \code -> do
      setStatus $ mkStatus code "Unknown Code"
      text "Error"
    get ("debug" <//> "route" <//> wildcard) $ \_ -> do
      req <- request
      text $ T.pack $ show req
    get "version" $ do
      text $ T.pack showVersion
    get "rooms" $ do
      trooms <- stateRooms <$> getState
      count <- liftIO . atomically $ IM.size <$> readTVar trooms
      text . T.pack . show $ count

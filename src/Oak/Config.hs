module Oak.Config where

import Data.Aeson
import Control.Monad

data Config
  = Config
  { cardDatabasePath :: FilePath
  , serverPort :: Int
  , serverHost :: String
  }

instance FromJSON Config where
    parseJSON (Object v) = Config 
                        <$> v .: "card_database" 
                        <*> v .: "port"
                        <*> v .: "host"
    parseJSON _ = mzero

module Main where

import Data.Aeson

import Oak.Web
import Oak.Config
import System.IO
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
  ecfg <- eitherDecode <$> BS.readFile "config.json"
  case ecfg of
    Left e -> hPutStrLn stderr ("Failed to parse config file:\n" ++ e)
    Right cfg -> do
      db <- eitherDecode <$> BS.readFile (cardDatabasePath cfg)
      case db of
        Left e -> hPutStrLn stderr e
        Right db' -> do
          runApp cfg db'

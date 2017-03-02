module Main where

import Data.Aeson

import Oak.Web
import Oak.Config
import System.IO
import System.Environment
import System.Directory
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
  argv <- getArgs
  (opts, _) <- compilerOpts argv
  args <- getSettings opts
  ecfg <- eitherDecode <$> BS.readFile (argsConfig args)
  case ecfg of
    Left e -> hPutStrLn stderr ("Failed to parse config file:\n" ++ e)
    Right cfg -> do
      maybe (return ()) (setCurrentDirectory) (cfg_root cfg)
      db <- eitherDecode <$> BS.readFile ("db.json")
      case db of
        Left e -> hPutStrLn stderr e
        Right db' -> do
          runApp cfg db'

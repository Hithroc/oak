module Main where

import Data.Aeson

import Oak.Web
import Oak.Config
import Oak.Core.Booster
import Control.Monad
import System.IO
import System.Environment
import System.Directory
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as S

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
      db <- fmap runJMeta . eitherDecode <$> BS.readFile ("data/db.json")
      cycles <- eitherDecode <$> BS.readFile ("data/cycles.json")
      case db >>= \y -> fmap (\x -> (y,x)) cycles of
        Left e -> hPutStrLn stderr e
        Right ((fcards :: [(String,Value)], db'), cycles') -> do
          (\(CardDatabase x) -> putStrLn $ "Total cards in the database: " ++ show (length x)) db'
          when (length fcards > 0) $ do
            hPutStrLn stderr $ "This is a list of failed cards:\n" ++ unlines (fmap show fcards)
            error $ "Failed to read " ++ show (length fcards) ++ " cards."
          runApp cfg (fixDatabase db') (convertBoosterCycles (fixDatabase db') cycles')

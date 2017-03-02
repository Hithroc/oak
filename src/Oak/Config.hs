{-# LANGUAGE TemplateHaskell #-}
module Oak.Config where

import Data.Aeson
import Control.Monad
import Data.Aeson.TH
import Paths_oak (version)
import qualified Data.Version as V
import System.Console.GetOpt
import System.Environment
import System.Exit

data Config
  = Config
  { cfg_root :: Maybe FilePath
  , cfg_port :: Int
  , cfg_host :: String
  }
$(deriveJSON defaultOptions { fieldLabelModifier = drop 4 } ''Config)

data Args
  = Args
  { argsConfig :: FilePath
  }

defaultArgs :: Args
defaultArgs
  = Args
  { argsConfig = "config.json"
  }

data Flag
  = ConfigFile FilePath
  | PrintVersion
  | PrintHelp

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"]     (NoArg PrintHelp)       "Print this message"
  , Option []    ["version"]  (NoArg PrintVersion)    "Print program's version"
  , Option []    ["config"] (ReqArg ConfigFile  "FILE") "Log file. Default is config.json"
  ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = case getOpt Permute options argv of
  (o,n,[]) -> return (o,n)
  (_,_,err) -> do
    putStrLn $ concat err
    printHelp
    exitFailure

printHelp :: IO ()
printHelp = do
  name <- getProgName
  let header = "Usage: " ++ name ++ " [OPTIONS]"
  putStrLn $ usageInfo header options

showVersion :: String
showVersion = "Oak ver. " ++ V.showVersion version

printVersion :: IO ()
printVersion = putStrLn showVersion

applyOpt :: Flag -> Args -> IO Args
applyOpt (ConfigFile p) s = return $ s { argsConfig = p }
applyOpt PrintVersion _ = do
  printVersion
  exitSuccess
applyOpt PrintHelp _ = do
  printHelp
  exitSuccess

getSettings :: [Flag] -> IO Args
getSettings = foldM (flip applyOpt) defaultArgs

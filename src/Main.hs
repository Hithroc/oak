module Main where

import Data.Aeson

import Oak.Web
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
  db <- eitherDecode <$> BS.readFile "db.json"
  case db of
    Left e -> print e
    Right db' -> do
      runApp db'

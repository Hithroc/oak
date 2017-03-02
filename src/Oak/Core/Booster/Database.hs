module Oak.Core.Booster.Database where

import Data.Aeson
import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V

import Oak.Core.Booster.Types

data CardDatabase = CardDatabase (S.Set Card)
  deriving Show

instance FromJSON CardDatabase where
  parseJSON (Object v) = do
    arr <- v .: "data"
    case arr of
      Array a -> do
          let 
            al :: [Value]
            al = V.toList a
          cs <- sequence $ map (\x -> (Just <$> parseJSON x) <|> return Nothing) al
          return . CardDatabase . S.fromList . catMaybes $ cs
      _ -> mzero
  parseJSON _ = mzero

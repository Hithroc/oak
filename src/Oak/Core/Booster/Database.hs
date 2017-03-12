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


-- Hardoced RR mappings
royalRareMapping :: [(Card -> Bool, Card -> Card)]
royalRareMapping =
  [ (matchCard EquestrianOdysseys "207", makeRR)
  , (matchCard EquestrianOdysseys "208", makeRR)

  , (matchCard HighMagic          "149", makeRR)
  , (matchCard HighMagic          "147", makeRR)
  , (matchCard HighMagic          "145", makeRR)

  , (matchCard MarksInTime        "139", makeRR)
  , (matchCard MarksInTime        "141", makeRR)
  ]
  where
    makeRR c = c { cardRarity = RoyalRare }
    matchCard exp num c = cardExpansion c == exp && cardNumber c == num

mapSet :: Ord a => [(a -> Bool, a -> a)] -> S.Set a -> S.Set a
mapSet [] s = s
mapSet ((p, f) : xs) s = (S.map f . S.filter p $ s) `S.union` mapSet xs s

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

module Oak.Core.Booster.Database where

import Data.Aeson
import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Either (partitionEithers)

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

fixDatabase :: CardDatabase -> CardDatabase
fixDatabase (CardDatabase db) = CardDatabase . mapSet royalRareMapping $ db

data JMeta a b = JMeta a b
runJMeta :: JMeta a b -> (a, b)
runJMeta (JMeta a b) = (a, b)
toJMeta :: (a, b) -> JMeta a b
toJMeta = uncurry JMeta

instance FromJSON (JMeta [(String, Value)] CardDatabase) where
  parseJSON (Object v) = do
    arr <- v .: "data"
    case arr of
      Array a -> do
          let 
            al :: [Value]
            al = V.toList a
            fromJSONCard v = case fromJSON v of
              Error s -> Left (s, v)
              Success x -> Right x
          return . toJMeta . fmap (CardDatabase . S.fromList) . partitionEithers . map fromJSONCard $ al
      _ -> mzero
  parseJSON _ = mzero

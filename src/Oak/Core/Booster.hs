module Oak.Core.Booster (module X, generateBooster) where

import Oak.Core.Booster.Types as X
import Oak.Core.Booster.Database as X
import Oak.Core.Booster.Cycles

import Control.Monad.Random
import Data.Ratio
import Control.Spoon
import qualified Data.Set as S
import Data.List

cloudHatePredicate :: forall a. a -> Bool
cloudHatePredicate = const True

boosterWeights :: BoosterType -> [(Rarity, Rational)]
boosterWeights PremiereBooster = [(UltraRare, 1 % 13)]
boosterWeights CanterlotNightsBooster = [(UltraRare, 1 % 11)]
boosterWeights TheCrystalGamesBooster = [(UltraRare, 1 % 11)]
boosterWeights AbsoluteDiscordBooster = [(UltraRare, 1 % 11)]
boosterWeights EquestrianOdysseysBooster
  = [ (UltraRare, 4 % 36)
    , (SuperRare, 8 % 36)
    , (RoyalRare, 1 % 216)
    ]
boosterWeights HighMagicBooster
  = [ (UltraRare, 4 % 36)
  , (SuperRare, 8 % 36)
  , (RoyalRare, 1 % 216)
  ]

boosterWeights MarksInTimeBooster
  = [ (UltraRare, 4 % 36)
    , (SuperRare, 8 % 36)
    , (RoyalRare, 1 % 216)
    ]
boosterWeights (CustomBooster _ x) = x

filterExpansion :: Expansion -> S.Set Card -> S.Set Card
filterExpansion expansion = S.filter ((==expansion) . cardExpansion)

isRarity :: Rarity -> Card -> Bool
isRarity r = (==r) . cardRarity

boosterCards :: BoosterType -> S.Set Card -> S.Set Card
boosterCards (CustomBooster cards _)   = const cards
boosterCards PremiereBooster           = filterExpansion Premiere
boosterCards CanterlotNightsBooster    = filterExpansion CanterlotNights
boosterCards TheCrystalGamesBooster    = filterExpansion TheCrystalGames
boosterCards AbsoluteDiscordBooster    = filterExpansion AbsoluteDiscord
boosterCards EquestrianOdysseysBooster = filterExpansion EquestrianOdysseys
boosterCards HighMagicBooster          = filterExpansion HighMagic
boosterCards MarksInTimeBooster        = filterExpansion MarksInTime

pick :: (MonadRandom m) => S.Set a -> m (Maybe a)
pick xs = do
  index <- getRandomR (0, S.size xs - 1)
  return . teaspoon . S.elemAt index $ xs

pickPred :: (MonadRandom m, Ord a) => [a -> Bool] -> S.Set a -> m [Maybe a]
pickPred [] _ = return []
pickPred (p:ps) xs
  | S.null xs = return $ map (const Nothing) (p:ps)
  | otherwise = do
  c <- pick (S.filter p xs)
  fmap (c:) (pickPred ps (maybe id (S.delete) c xs))

pickRarity :: MonadRandom m => [(Rarity, Rational)] -> m Rarity
pickRarity xs = fromList $ (Common, comr) : xs -- MonadRandom's fromList
  where comr = max 0 . foldl (-) 1 . map snd $ xs

generateBooster :: MonadRandom m => CardDatabase -> BoosterCycles -> BoosterType -> m [Card]
generateBooster (CardDatabase cards) bcycles btype = do
  r <- pickRarity (boosterWeights btype)
  let rarities = replicate 7 Common ++ [r, Rare] ++ replicate 3 Uncommon
      groupedRarities :: [(Int, Rarity)]
      groupedRarities = fmap (\x -> (length x, head x)) . group $ rarities
      getCycle :: MonadRandom m => Rarity -> m CardCycle
      getCycle rar = case lookupCycle btype rar bcycles of
                   Nothing -> makeUniformCycle (boosterCards btype cards)
                   Just cycle -> return cycle
  (cycles :: [(Int, CardCycle)]) <- traverse (traverse (randomizeCycle <=< getCycle)) groupedRarities
  return . concat . map fst . getCycledBooster $ cycles

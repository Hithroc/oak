module Oak.Core.Booster (module X, generateBox, boxStream) where

import Oak.Core.Booster.Types as X
import Oak.Core.Booster.Database as X
import Oak.Core.Booster.Cycles as X

import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Reader
import Data.Ratio
import qualified Data.Map as M
import Data.List
import System.Random.Shuffle
import qualified Data.Stream.Infinite as S

cloudHatePredicate :: forall a. a -> Bool
cloudHatePredicate = const True

pickRarity :: MonadRandom m => [(Rarity, Rational)] -> m Rarity
pickRarity xs = fromList $ (Common, comr) : xs -- MonadRandom's fromList
  where comr = max 0 . foldl (-) 1 . map snd $ xs

generateBox :: MonadRandom m => CardDatabase -> BoosterCycles -> BoosterType -> m [[Card]]
generateBox (CardDatabase cards) bcycles btype = do
  boxStruct <- constructBox btype
  let
    cycles = maybe M.empty id $ M.lookup btype bcycles
    groupRarities rars = fmap (\x -> (length x, head x)) . group $ rars
  cycles' <- traverse randomizeCycle cycles
  let go = shuffleM <=< traverse (getCycledBooster . groupRarities) $ boxStruct
  evalStateT (runReaderT go (boosterCards btype cards)) cycles'

regularBooster :: Rarity -> [Rarity]
regularBooster Common = replicate 8 Common ++ [Rare] ++ replicate 3 Uncommon
regularBooster r = replicate 7 Common ++ [Rare, r] ++ replicate 3 Uncommon

constructBox :: MonadRandom m => BoosterType -> m [[Rarity]]
constructBox bt
  | bt == PremiereBooster
    = do
    r <- pickRarity [(UltraRare, 77 % 100)]
    let quadrant rar = Common : Common : rar : replicate 6 Common
    return . map regularBooster . concat $ quadrant r : quadrant Common : replicate 2 (quadrant UltraRare)

  | bt `elem` [CanterlotNightsBooster, TheCrystalGamesBooster, AbsoluteDiscordBooster]
    = do
    r <- pickRarity [(UltraRare, 27 % 100)]
    let quadrant rar = Common : Common : rar : replicate 6 Common
    return . map regularBooster . concat $ quadrant r : replicate 3 (quadrant UltraRare)

  | otherwise
    = do
    r <- pickRarity [(RoyalRare, 1 % 6)]
    let quadrant = SuperRare : Common : UltraRare : Common : SuperRare : replicate 4 Common
        specialQuadrant = SuperRare : Common : UltraRare : Common : SuperRare : Common : r : replicate 2 Common
    return . map regularBooster . concat $ replicate 3 quadrant ++ [specialQuadrant]

boxStream :: MonadRandom m => CardDatabase -> BoosterCycles -> BoosterType -> m (S.Stream [Card])
boxStream db bcycles btype = do
  box <- generateBox db bcycles btype
  S.prepend box <$> boxStream db bcycles btype

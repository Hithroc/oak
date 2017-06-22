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

pickSequence' :: MonadRandom m => a -> [(a, Rational)] -> m a
pickSequence' def xs = fromList $ (def, comr) : xs -- MonadRandom's fromList
  where comr = max 0 . foldl (-) 1 . map snd $ xs

pickSequence :: MonadRandom m => [(PackSequence, Rational)] -> m PackSequence
pickSequence = pickSequence' FrontCommonSeq

generateBox :: MonadRandom m => CardDatabase -> BoosterCycles -> BoosterType -> m [[Card]]
generateBox (CardDatabase cards) bcycles btype = do
  boxStruct <- constructBox btype
  let
    cycles = maybe M.empty id $ M.lookup btype bcycles
    groupRarities rars = fmap (\x -> (length x, head x)) . group $ rars
  cycles' <- traverse randomizeCycle cycles
  let go = shuffleM <=< traverse (getCycledBooster . groupRarities) $ boxStruct
  evalStateT (runReaderT go (boosterCards btype cards)) cycles'

regularBooster :: PackSequence -> [PackSequence]
regularBooster FrontCommonSeq = replicate 8 FrontCommonSeq ++ [RareSeq] ++ replicate 3 UncommonSeq
regularBooster r = replicate 7 FrontCommonSeq ++ [RareSeq, r] ++ replicate 3 UncommonSeq

defendersBooster :: Bool -> PackSequence -> [PackSequence]
defendersBooster shift seq = replicate (3+shiftFront) FrontCommonSeq ++ [seq] ++ [RareSeq] ++ replicate 3 UncommonSeq ++ replicate (3+shiftBack) BackCommonSeq
  where
    shiftBool x = if x then 1 else 0
    shiftFront = shiftBool shift
    shiftBack = shiftBool (not shift)

constructBox :: MonadRandom m => BoosterType -> m [[PackSequence]]
constructBox bt
  | bt == PremiereBooster
    = do
    r <- pickSequence [(UltraRareSeq, 77 % 100)]
    let quadrant rar = FrontCommonSeq : FrontCommonSeq : rar : replicate 6 FrontCommonSeq
    return . map regularBooster . concat $ quadrant r : quadrant FrontCommonSeq : replicate 2 (quadrant UltraRareSeq)

  | bt `elem` [CanterlotNightsBooster, TheCrystalGamesBooster, AbsoluteDiscordBooster]
    = do
    r <- pickSequence [(UltraRareSeq, 27 % 100)]
    let quadrant rar = FrontCommonSeq : FrontCommonSeq : rar : replicate 6 FrontCommonSeq
    return . map regularBooster . concat $ quadrant r : replicate 3 (quadrant UltraRareSeq)

  | bt `elem` [DefendersOfEquestriaBooster]
    = do
      let packSeq x = [(x, SuperRareSeq), (False, FrontCommonSeq), (not x, UltraRareSeq), (False, FrontCommonSeq), (False, FrontCommonSeq), (x, SuperRareSeq), (False, FrontCommonSeq), (False, FrontCommonSeq), (False, FrontCommonSeq)]
      return . map (uncurry defendersBooster) . concat . zipWith (flip ($)) (cycle [False, True]) . replicate 4 $ packSeq


  | otherwise
    = do
    r <- pickSequence [(RoyalRareSeq, 1 % 6)]
    let quadrant = SuperRareSeq : FrontCommonSeq : UltraRareSeq : FrontCommonSeq : SuperRareSeq : replicate 4 FrontCommonSeq
        specialQuadrant = SuperRareSeq : FrontCommonSeq : UltraRareSeq : FrontCommonSeq : SuperRareSeq : FrontCommonSeq : r : replicate 2 FrontCommonSeq
    return . map regularBooster . concat $ replicate 3 quadrant ++ [specialQuadrant]

boxStream :: MonadRandom m => CardDatabase -> BoosterCycles -> BoosterType -> m (S.Stream [Card])
boxStream db bcycles btype = do
  box <- generateBox db bcycles btype
  S.prepend box <$> boxStream db bcycles btype

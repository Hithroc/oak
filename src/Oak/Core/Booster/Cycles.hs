module Oak.Core.Booster.Cycles where

import Oak.Core.Booster.Types
import Oak.Core.Booster.Database
import qualified Data.Stream.Infinite as S
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NL
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Maybe
import Control.DeepSeq
import Data.Aeson
import Control.Monad
import Control.Monad.Random

data CardCycle = CardCycle Int (S.Stream Card)
instance Show CardCycle where
  show (CardCycle n cycle) = "CardCycle " ++ show n ++ " " ++ show (S.take n cycle)
data JSONCardCycle = JSONCardCycle (NL.NonEmpty Text)
  deriving Show
type RarityCycles = M.Map Rarity CardCycle
type BoosterCycles = M.Map BoosterType RarityCycles

lookupCycle :: BoosterType -> Rarity -> BoosterCycles -> Maybe CardCycle
lookupCycle btype rar = M.lookup btype >=> M.lookup rar

getCycledBooster :: [(Int, CardCycle)] -> [([Card], CardCycle)]
getCycledBooster xs = foldr (\(n, (CardCycle len cycle)) acc -> fmap (CardCycle len) (S.splitAt n cycle) : acc) [] xs

instance FromJSON JSONCardCycle where
  parseJSON (Array v) = do
    l <- sequence . V.toList . fmap parseJSON $ v
    maybe mzero (return . JSONCardCycle) . NL.nonEmpty $ l
  parseJSON _ = mzero
 
convertJsonCycle :: Set.Set Card -> JSONCardCycle -> CardCycle
convertJsonCycle db (JSONCardCycle cycle) = CardCycle (length cycle) . S.cycle . force . fmap lookupDb $ cycle
  where
    lookupDb n = head . filter ((==n) . cardNumber) . Set.toList $ db


filterExpansion :: Expansion -> Set.Set Card -> Set.Set Card
filterExpansion expansion = Set.filter ((==expansion) . cardExpansion)

boosterCards :: BoosterType -> Set.Set Card -> Set.Set Card
boosterCards (CustomBooster cards _)   = const cards
boosterCards PremiereBooster           = filterExpansion Premiere
boosterCards CanterlotNightsBooster    = filterExpansion CanterlotNights
boosterCards TheCrystalGamesBooster    = filterExpansion TheCrystalGames
boosterCards AbsoluteDiscordBooster    = filterExpansion AbsoluteDiscord
boosterCards EquestrianOdysseysBooster = filterExpansion EquestrianOdysseys
boosterCards HighMagicBooster          = filterExpansion HighMagic
boosterCards MarksInTimeBooster        = filterExpansion MarksInTime

convertBoosterCycles :: CardDatabase -> M.Map BoosterType (M.Map Rarity JSONCardCycle) -> BoosterCycles
convertBoosterCycles (CardDatabase db) = M.mapWithKey (\e v -> fmap (convertJsonCycle (boosterCards e db)) v)

randomizeCycle :: MonadRandom m => CardCycle -> m CardCycle
randomizeCycle c@(CardCycle len cycle) = if len <= 0 then return c else do
  n <- getRandomR (0, len)
  return $ CardCycle n (S.drop n cycle)

makeUniformCycle :: MonadRandom m => Set.Set Card -> m CardCycle
makeUniformCycle cards = do
  stream <- getRandomRs (0, Set.size cards - 1)
  return (CardCycle 0 (fmap (flip Set.elemAt cards) (S.cycle . NL.fromList $ stream)))

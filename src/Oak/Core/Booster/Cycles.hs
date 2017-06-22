module Oak.Core.Booster.Cycles where

import Oak.Core.Booster.Types
import Oak.Core.Booster.Database
import qualified Data.Stream.Infinite as S
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NL
import qualified Data.Map as M
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import Control.DeepSeq
import Data.Aeson
import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Reader
import Text.Read (readMaybe)

data PackSequence
  = FrontCommonSeq
  | BackCommonSeq
  | RareSeq
  | UncommonSeq
  | SuperRareSeq
  | UltraRareSeq
  | RoyalRareSeq
  deriving (Read, Show, Eq, Ord)

instance FromJSON PackSequence where
  parseJSON (String txt) = maybe mzero return . readMaybe . unpack $ txt
  parseJSON _ = mzero
instance FromJSONKey PackSequence where
  fromJSONKey = FromJSONKeyTextParser (maybe mzero return . readMaybe . unpack)

sequenceToRarity :: PackSequence -> Rarity
sequenceToRarity FrontCommonSeq = Common
sequenceToRarity BackCommonSeq = Common
sequenceToRarity RareSeq = Rare
sequenceToRarity UncommonSeq = Uncommon
sequenceToRarity SuperRareSeq = SuperRare
sequenceToRarity UltraRareSeq = UltraRare
sequenceToRarity RoyalRareSeq = RoyalRare

data CardCycle = CardCycle Int (S.Stream Card)
instance Show CardCycle where
  show (CardCycle n cyc) = "CardCycle " ++ show n ++ " " ++ show (S.take n cyc)
data JSONCardCycle = JSONCardCycle (NL.NonEmpty Text)
  deriving Show
type RarityCycles = M.Map PackSequence CardCycle
type BoosterCycles = M.Map BoosterType RarityCycles

instance FromJSON JSONCardCycle where
  parseJSON (Array v) = do
    l <- sequence . V.toList . fmap parseJSON $ v
    maybe mzero (return . JSONCardCycle) . NL.nonEmpty $ l
  parseJSON _ = mzero

convertJsonCycle :: Set.Set Card -> JSONCardCycle -> CardCycle
convertJsonCycle db (JSONCardCycle cyc) = CardCycle (length cyc) . S.cycle . force . fmap lookupDb $ cyc
  where
    lookupDb n = head . filter ((==n) . cardNumber) . Set.toList $ db

splitCycle :: Int -> CardCycle -> ([Card], CardCycle)
splitCycle n (CardCycle len cyc) = fmap (CardCycle len) . S.splitAt n $ cyc

filterExpansion :: Expansion -> Set.Set Card -> Set.Set Card
filterExpansion expansion = Set.filter ((==expansion) . cardExpansion)

isRarity :: Rarity -> Card -> Bool
isRarity r = (==r) . cardRarity

boosterCards :: BoosterType -> Set.Set Card -> Set.Set Card
boosterCards (CustomBooster cards _)   = const cards
boosterCards PremiereBooster           = filterExpansion Premiere
boosterCards CanterlotNightsBooster    = filterExpansion CanterlotNights
boosterCards TheCrystalGamesBooster    = filterExpansion TheCrystalGames
boosterCards AbsoluteDiscordBooster    = filterExpansion AbsoluteDiscord
boosterCards EquestrianOdysseysBooster = filterExpansion EquestrianOdysseys
boosterCards HighMagicBooster          = filterExpansion HighMagic
boosterCards MarksInTimeBooster        = filterExpansion MarksInTime
boosterCards DefendersOfEquestriaBooster = filterExpansion DefendersOfEquestria

convertBoosterCycles :: CardDatabase -> M.Map BoosterType (M.Map PackSequence JSONCardCycle) -> BoosterCycles
convertBoosterCycles (CardDatabase db) = M.mapWithKey (\e v -> fmap (convertJsonCycle (boosterCards e db)) v)

randomizeCycle :: MonadRandom m => CardCycle -> m CardCycle
randomizeCycle c@(CardCycle len cyc) = if len <= 0 then return c else do
  n <- getRandomR (0, len)
  return $ CardCycle n (S.drop n cyc)

makeUniformCycle :: MonadRandom m => Set.Set Card -> m CardCycle
makeUniformCycle cards = do
  stream <- getRandomRs (0, Set.size cards - 1)
  return (CardCycle 0 (fmap (flip Set.elemAt cards) (S.cycle . NL.fromList $ stream)))

getCycledCards :: (MonadState RarityCycles m, MonadRandom m, MonadReader (Set.Set Card) m) => Int -> PackSequence -> m [Card]
getCycledCards n rar = do
  cycles <- get
  cards <- ask
  (ret, cyc) <- splitCycle n <$> maybe (makeUniformCycle . Set.filter (isRarity (sequenceToRarity rar)) $ cards) return (M.lookup rar cycles)
  modify (M.insert rar cyc)
  return ret

getCycledCard :: (MonadState RarityCycles m, MonadRandom m, MonadReader (Set.Set Card) m) => PackSequence -> m Card
getCycledCard = fmap head . getCycledCards 1

getCycledBooster :: (MonadState RarityCycles m, MonadRandom m, MonadReader (Set.Set Card) m) => [(Int, PackSequence)] -> m [Card]
getCycledBooster = fmap concat . traverse (uncurry getCycledCards)

{-# LANGUAGE TemplateHaskell #-}
module Oak.Core.Booster.Types where

import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Set as S
import Web.Internal.HttpApiData
import Control.Monad (mzero)
import Data.Aeson
import Data.SafeCopy
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Text.Read (readMaybe)
import Data.Maybe
import Data.Monoid

data Rarity -- Darling
  = Fixed
  | ParallelFoil
  | Promotional
  | Common
  | Uncommon
  | Rare
  | SuperRare
  | UltraRare
  | RoyalRare
  deriving (Eq, Ord, Read, Generic)
deriveSafeCopy 1 'base ''Rarity

instance FromJSONKey Rarity where
  fromJSONKey = FromJSONKeyTextParser (maybe mzero return . readMaybe . T.unpack)

instance NFData Rarity

instance Show Rarity where
  show Common      = "C"
  show Uncommon    = "U"
  show Rare        = "R"
  show SuperRare   = "SR"
  show UltraRare   = "UR"
  show RoyalRare   = "RR"
  show Fixed       = "F"
  show ParallelFoil = "ƒ"
  show Promotional = "Pƒ"

data Expansion
  = Premiere
  | CanterlotNights
  | TheCrystalGames
  | AbsoluteDiscord
  | EquestrianOdysseys
  | HighMagic
  | MarksInTime
  | RockNRave
  | CelestialSolstice
  | GenericFixed
  | SandsOfTime
  | DefendersOfEquestria
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 1 'base ''Expansion

instance NFData Expansion

data Color
  = None
  | Wild
  | Blue
  | White
  | Orange
  | Purple
  | Yellow
  | Pink
  | Rainbow
  deriving (Eq, Ord, Show, Read, Generic)
deriveSafeCopy 1 'base ''Color
instance NFData Color

data CardType
  = Mane
  | Problem
  | Resource
  | Event
  | Troublemaker
  | Friend
  deriving (Eq, Ord, Read, Show, Generic)
deriveSafeCopy 1 'base ''CardType
instance NFData CardType

data CardOld =
  CardOld
  { oldCardRarity :: Rarity
  , oldCardExpansion :: Expansion
  , oldCardName :: Text
  , oldCardNumber :: Text
  }
  deriving (Eq, Ord, Read, Generic)
deriveSafeCopy 1 'base ''CardOld

instance NFData CardOld

data Card =
  Card
  { cardRarity :: Rarity
  , cardExpansion :: Expansion
  , cardName :: Text
  , cardNumber :: Text
  , cardReq :: Maybe [Int]
  , cardColors :: [Color]
  , cardSecReq :: Maybe Int
  , cardWildReq :: Maybe Int
  , cardCost :: Maybe Int
  , cardType :: CardType
  , cardPower :: Maybe Int
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance Migrate Card where
  type MigrateFrom Card = CardOld
  migrate c
    = Card
    { cardRarity = oldCardRarity c
    , cardExpansion = oldCardExpansion c
    , cardName = oldCardName c
    , cardNumber = oldCardNumber c
    , cardReq = Nothing
    , cardColors = []
    , cardSecReq = Nothing
    , cardWildReq = Nothing
    , cardCost = Nothing
    , cardType = Friend
    , cardPower = Nothing
    }

deriveSafeCopy 2 'extension ''Card

instance NFData Card

data BoosterType
  = PremiereBooster
  | CanterlotNightsBooster
  | TheCrystalGamesBooster
  | AbsoluteDiscordBooster
  | EquestrianOdysseysBooster
  | HighMagicBooster
  | MarksInTimeBooster
  | DefendersOfEquestriaBooster
  | CustomBooster (S.Set Card) [(Rarity, Rational)]
  deriving (Eq, Show, Read, Ord)
deriveSafeCopy 1 'base ''BoosterType

textToBooster :: T.Text -> Maybe BoosterType
textToBooster "Premiere" = Just PremiereBooster
textToBooster "CanterlotNights" = Just CanterlotNightsBooster
textToBooster "TheCrystalGames" = Just TheCrystalGamesBooster
textToBooster "AbsoluteDiscord" = Just AbsoluteDiscordBooster
textToBooster "EquestrianOdysseys" = Just EquestrianOdysseysBooster
textToBooster "HighMagic" = Just HighMagicBooster
textToBooster "MarksInTime" = Just MarksInTimeBooster
textToBooster "DefendersOfEquestria" = Just DefendersOfEquestriaBooster
textToBooster _ = Nothing

instance FromJSON BoosterType where
  parseJSON (String s) = maybe mzero return $ textToBooster s
  parseJSON _ = mzero

instance FromJSONKey BoosterType where
  fromJSONKey = FromJSONKeyTextParser (maybe mzero return . textToBooster)

setToLetters :: Expansion -> String
setToLetters Premiere           = "pr"
setToLetters CanterlotNights    = "cn"
setToLetters TheCrystalGames    = "cg"
setToLetters AbsoluteDiscord    = "ad"
setToLetters EquestrianOdysseys = "eo"
setToLetters HighMagic          = "hm"
setToLetters MarksInTime        = "mt"
setToLetters DefendersOfEquestria = "de"
setToLetters _ = "uk" -- Change this later

lettersToSet :: String -> Maybe Expansion
lettersToSet "pr" = Just Premiere
lettersToSet "cn" = Just CanterlotNights
lettersToSet "cg" = Just TheCrystalGames
lettersToSet "ad" = Just AbsoluteDiscord
lettersToSet "eo" = Just EquestrianOdysseys
lettersToSet "hm" = Just HighMagic
lettersToSet "mt" = Just MarksInTime
lettersToSet "de" = Just DefendersOfEquestria
lettersToSet _ = Nothing

instance FromHttpApiData BoosterType where
  parseUrlPiece str
    | str == "pr" = Right PremiereBooster
    | str == "cn" = Right CanterlotNightsBooster
    | str == "cg" = Right TheCrystalGamesBooster
    | str == "ad" = Right AbsoluteDiscordBooster
    | str == "eo" = Right EquestrianOdysseysBooster
    | str == "hm" = Right HighMagicBooster
    | str == "mt" = Right MarksInTimeBooster
    | str == "de" = Right DefendersOfEquestriaBooster
    | otherwise = Left "Unknown set!"
  parseQueryParam = parseUrlPiece

instance FromHttpApiData [BoosterType] where
  parseUrlPiece str = sequence . map (parseUrlPiece) . T.splitOn "," $ str

instance FromJSON Rarity where
  parseJSON (String "C")  = return Common
  parseJSON (String "U")  = return Uncommon
  parseJSON (String "R")  = return Rare
  parseJSON (String "SR") = return SuperRare
  parseJSON (String "UR") = return UltraRare
  parseJSON (String "RR") = return RoyalRare
  parseJSON (String "F")  = return Fixed
  parseJSON (String "P")  = return Promotional
  parseJSON (String s) = fail . T.unpack $ "Unknown rarity: " <> s
  parseJSON _ = mzero

instance FromJSON Expansion where
  parseJSON (String "PR") = return Premiere
  parseJSON (String "CN") = return CanterlotNights
  parseJSON (String "CG") = return TheCrystalGames
  parseJSON (String "AD") = return AbsoluteDiscord
  parseJSON (String "EO") = return EquestrianOdysseys
  parseJSON (String "HM") = return HighMagic
  parseJSON (String "MT") = return MarksInTime
  parseJSON (String "RR") = return RockNRave
  parseJSON (String "CS") = return CelestialSolstice
  parseJSON (String "ST") = return SandsOfTime
  parseJSON (String "GF" ) = return GenericFixed
  parseJSON (String "DE" ) = return DefendersOfEquestria
  parseJSON (String s) = fail . T.unpack $ "Unknown set: " <> s
  parseJSON _ = mzero

readSplit :: Read a => String -> Maybe [a]
readSplit "" = return []
readSplit str = do
  rx <- readMaybe x
  rxs <- readSplit str'
  return (rx:rxs)
  where
    (x, str') = safeTail <$> break (=='/') str
    safeTail [] = []
    safeTail (_:xs) = xs

instance FromJSON Card where
  parseJSON (Object v) = Card
                      <$> v .: "rarity"
                      <*> v .: "set"
                      <*> v .: "fullname"
                      <*> v .: "number"
                      <*> ((>>= readSplit) <$> v .:? "req")
                      <*> ((fromMaybe [] . readSplit) <$> v .: "color")
                      <*> v .: "secreq"
                      <*> v .: "wildreq"
                      <*> v .: "cost"
                      <*> (maybe mzero return . readMaybe =<< v .: "type")
                      <*> ((>>=readMaybe) <$> v .:? "power")
  parseJSON _ = mzero

{-# LANGUAGE TemplateHaskell #-}
module Oak.Core.Booster.Types where

import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Set as S
import Web.Internal.HttpApiData
import Control.Monad (mzero)
import Data.Aeson
import Data.SafeCopy

data Rarity -- Darling
  = Fixed
  | Promotional
  | Common
  | Uncommon
  | Rare
  | SuperRare
  | UltraRare
  | RoyalRare
  deriving (Eq, Ord)
deriveSafeCopy 1 'base ''Rarity

instance Show Rarity where
  show Common      = "C"
  show Uncommon    = "U"
  show Rare        = "R"
  show SuperRare   = "SR"
  show UltraRare   = "UR"
  show RoyalRare   = "RR"
  show Fixed       = "F"
  show Promotional = "P"


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
  deriving (Eq, Ord, Show)
deriveSafeCopy 1 'base ''Expansion

data Card =
  Card
  { cardRarity :: Rarity
  , cardExpansion :: Expansion
  , cardName :: Text
  , cardNumber :: Text
  }
  deriving (Eq, Ord)
deriveSafeCopy 1 'base ''Card

instance Show Card where
  show (Card r s n i) = show s ++ " " ++ show i ++ " " ++ show r ++ " " ++ unpack n


data BoosterType
  = PremiereBooster
  | CanterlotNightsBooster
  | TheCrystalGamesBooster
  | AbsoluteDiscordBooster
  | EquestrianOdysseysBooster
  | HighMagicBooster
  | MarksInTimeBooster
  | CustomBooster (S.Set Card) [(Rarity, Rational)]
  deriving (Eq, Show)
deriveSafeCopy 1 'base ''BoosterType

setToLetters :: Expansion -> String
setToLetters Premiere           = "pr"
setToLetters CanterlotNights    = "cn"
setToLetters TheCrystalGames    = "cg"
setToLetters AbsoluteDiscord    = "ad"
setToLetters EquestrianOdysseys = "eo"
setToLetters HighMagic          = "hm"
setToLetters MarksInTime        = "mt"
setToLetters _ = "uk" -- Change this later

lettersToSet :: String -> Maybe Expansion
lettersToSet "pr" = Just Premiere
lettersToSet "cn" = Just CanterlotNights
lettersToSet "cg" = Just TheCrystalGames
lettersToSet "ad" = Just AbsoluteDiscord
lettersToSet "eo" = Just EquestrianOdysseys
lettersToSet "hm" = Just HighMagic
lettersToSet "mt" = Just MarksInTime
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
    | otherwise = Left "Unknown set!"
  parseQueryParam = parseUrlPiece

instance FromHttpApiData [BoosterType] where
  parseUrlPiece str = sequence . map (parseUrlPiece) . T.splitOn "," $ str

instance FromJSON Rarity where
  parseJSON (String "C")  = return Common
  parseJSON (String "U") = return Uncommon
  parseJSON (String "R")  = return Rare
  parseJSON (String "SR") = return SuperRare
  parseJSON (String "UR") = return UltraRare
  parseJSON (String "RR") = return RoyalRare
  parseJSON (String "F")  = return Fixed
  parseJSON (String "P")  = return Promotional
  parseJSON _ = mzero

instance FromJSON Expansion where
  parseJSON (String "PR") = return Premiere
  parseJSON (String "CN") = return CanterlotNights
  parseJSON (String "CG") = return TheCrystalGames
  parseJSON (String "AD") = return AbsoluteDiscord
  parseJSON (String "EO") = return EquestrianOdysseys
  parseJSON (String "HM") = return HighMagic
  parseJSON (String "MT") = return MarksInTime
  parseJSON _ = mzero

instance FromJSON Card where
  parseJSON (Object v) = Card
                      <$> v .: "rarity"
                      <*> v .: "set"
                      <*> v .: "fullname"
                      <*> v .: "number"
  parseJSON _ = mzero

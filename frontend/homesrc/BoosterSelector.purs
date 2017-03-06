module BoosterSelector where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core(AttrName(..), ClassName(..))
import Data.Maybe
import Data.Array((!!))


data BoosterType
  = Premiere
  | CanterlotNights
  | TheCrystalGames
  | AbsoluteDiscord
  | EquestrianOdysseys
  | HighMagic
  | MarksInTime

instance showBoosterType :: Show BoosterType where
  show Premiere = "Premiere"
  show CanterlotNights = "Canterlot Nights"
  show TheCrystalGames = "The Crystal Games"
  show AbsoluteDiscord = "Absolute Discord"
  show EquestrianOdysseys = "Equestrian Odysseys"
  show HighMagic = "High Magic"
  show MarksInTime = "Marks in Time"

boosterTypes = [Premiere, CanterlotNights, TheCrystalGames, AbsoluteDiscord, EquestrianOdysseys, HighMagic, MarksInTime]

setToLetters :: BoosterType -> String
setToLetters Premiere           = "pr"
setToLetters CanterlotNights    = "cn"
setToLetters TheCrystalGames    = "cg"
setToLetters AbsoluteDiscord    = "ad"
setToLetters EquestrianOdysseys = "eo"
setToLetters HighMagic          = "hm"
setToLetters MarksInTime        = "mt"
setToLetters _ = "uk" -- Change this later

type BoosterSelectorState
  = { booster :: BoosterType
    }

data BoosterSelectorQuery a
  = GetBooster (BoosterType -> a)
  | SetBooster Int a

boosterSelector :: forall m. H.Component HH.HTML BoosterSelectorQuery Unit Void m
boosterSelector =
  H.component
  { initialState : const initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where
    initialState :: BoosterSelectorState
    initialState = { booster : Premiere }

    render :: BoosterSelectorState -> H.ComponentHTML BoosterSelectorQuery
    render st =
      HH.select [HE.onSelectedIndexChange (HE.input SetBooster), HP.class_ (ClassName $ show st.booster)] $
      map (\x -> HH.option_ [HH.text $ show x]) boosterTypes

    eval :: BoosterSelectorQuery ~> H.ComponentDSL BoosterSelectorState BoosterSelectorQuery Void m
    eval (GetBooster reply) = do
      st <- H.get
      pure (reply st.booster)
    eval (SetBooster ix next) = do
      case boosterTypes !! ix of
        Just b -> H.modify $ _ { booster = b }
        Nothing -> pure unit
      pure next

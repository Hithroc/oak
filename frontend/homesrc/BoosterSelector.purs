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
derive instance eqBoosterType :: Eq BoosterType

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
  | SetBoosterType BoosterType a

boosterSelector :: forall m. H.Component HH.HTML BoosterSelectorQuery BoosterType Void m
boosterSelector =
  H.component
  { initialState : initialState
  , render
  , eval
  , receiver: HE.input SetBoosterType
  }
  where
    initialState :: BoosterType -> BoosterSelectorState
    initialState bt = { booster : bt }

    render :: BoosterSelectorState -> H.ComponentHTML BoosterSelectorQuery
    render st =
      HH.select [HE.onSelectedIndexChange (HE.input SetBooster), HP.class_ (ClassName $ show st.booster)] $
      map (\x -> HH.option (if x == st.booster then [HP.selected true] else []) [HH.text $ show x]) boosterTypes

    eval :: BoosterSelectorQuery ~> H.ComponentDSL BoosterSelectorState BoosterSelectorQuery Void m
    eval (GetBooster reply) = do
      st <- H.get
      pure (reply st.booster)
    eval (SetBoosterType bt next) = do
      H.modify $ _ { booster = bt }
      pure next
    eval (SetBooster ix next) = do
      case boosterTypes !! ix of
        Just b -> H.modify $ _ { booster = b }
        Nothing -> pure unit
      pure next

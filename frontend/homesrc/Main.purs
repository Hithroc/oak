module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core(AttrName(..), ClassName(..))
import Halogen.Component.ChildPath as CP
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as AX
import Data.Map as M
import Data.Maybe(Maybe(..), maybe)
import Data.Traversable(sequence)
import Data.Const
import Network.HTTP.StatusCode
import DOM.HTML.Types(WINDOW)
import DOM (DOM)
import DOM.HTML(window)
import DOM.HTML.Window(open)
import BoosterSelector
import Data.Array (mapWithIndex, replicate)
import Data.Foldable (intercalate)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (throwException, error)
import Data.Int (fromString)
import Data.Array as A
import Data.NonEmpty ((:|))
import Data.InfiniteList as IL
import Data.Tuple


type ManeState
  = { boosters :: Int
    , roomCount :: Maybe Int
    , oakVersion :: Maybe String
    , roomType :: RoomType
    }

data RoomType
  = Draft
  | Sealed
  | BoosterBox

packLimits :: RoomType -> Tuple Int Int
packLimits Draft = Tuple 4 8
packLimits Sealed = Tuple 8 8
packLimits BoosterBox = Tuple 1 1

data ManeQuery a
  = AddBooster a
  | RemoveBooster a
  | StartGame a
  | SetRoomType RoomType a
  | Initialize a

type Slot = Int

type ManeAff eff = Aff (ajax :: AX.AJAX, console :: CONSOLE, window :: WINDOW, dom :: DOM | eff)
mane :: forall m. H.Component HH.HTML ManeQuery Unit Void (ManeAff m)
mane =
  H.lifecycleParentComponent
  { initialState : const { boosters : 4, roomCount : Nothing, oakVersion : Nothing, roomType : Draft}
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  , receiver : const Nothing
  }
  where
    boosterStream :: IL.InfiniteList (Tuple Int BoosterType) BoosterType
    boosterStream = IL.repeat $ EquestrianOdysseys :| [HighMagic, MarksInTime, MarksInTime, EquestrianOdysseys, EquestrianOdysseys, HighMagic, MarksInTime]
    render :: ManeState -> H.ParentHTML ManeQuery BoosterSelectorQuery Int (ManeAff m)
    render st =
      HH.div [ HP.class_ (ClassName "create-room-block")] $
      [ HH.div [HP.class_ (ClassName "roomcount")] [ HH.text $ "Number of active rooms: " <> maybe "0" show st.roomCount ]
      , HH.button [ HE.onClick (HE.input_ StartGame) ] [ HH.text "Create Room" ]
      , HH.div [HP.class_ (ClassName "roomtype-selector") ]
        [ HH.div_
          [ HH.input [ HP.checked true, HP.name "roomtype", HP.type_ HP.InputRadio, HE.onClick (HE.input_ $ SetRoomType Draft) ]
          , HH.br_
          , HH.label_ [HH.text "Draft"]
          ]
        , HH.div_
          [ HH.input [ HP.name "roomtype", HP.type_ HP.InputRadio, HE.onClick (HE.input_ $ SetRoomType Sealed) ]
          , HH.br_
          , HH.label_ [HH.text "Sealed"]
          ]
        , HH.div_
          [ HH.input [ HP.name "roomtype", HP.type_ HP.InputRadio, HE.onClick (HE.input_ $ SetRoomType BoosterBox) ]
          , HH.br_
          , HH.label_ [HH.text "Booster box"]
          ]
        ]
      , HH.div [HP.class_ (ClassName "booster-buttons")]
        [ HH.button [ HP.disabled $ st.boosters >= snd (packLimits st.roomType), HE.onClick (HE.input_ AddBooster) ] [ HH.text "Add pack" ]
        , HH.button [ HP.disabled $ st.boosters <= 1, HE.onClick (HE.input_ RemoveBooster) ] [ HH.text "Remove pack" ]
        ]
      ]
      <> mapWithIndex (\i x -> HH.slot i boosterSelector x absurd) (IL.take st.boosters boosterStream)
      <> [ HH.div [HP.class_ (ClassName "version")] [ HH.text $ maybe "Version unknown" id st.oakVersion ] ]

    eval :: ManeQuery ~> H.ParentDSL ManeState ManeQuery BoosterSelectorQuery Slot Void (ManeAff m)
    eval (Initialize next) = do
      rooms <- H.liftAff $ AX.get ("/rooms")
      vers <- H.liftAff $ AX.get ("/version")
      H.modify $ _ { roomCount = fromString rooms.response, oakVersion = Just $ vers.response }
      pure next

    eval (AddBooster next) = do
      st <- H.get
      unless (st.boosters >= snd (packLimits st.roomType)) $ H.modify $ \s -> s { boosters = s.boosters + 1 }
      pure next

    eval (RemoveBooster next) = do
      st <- H.get
      unless (st.boosters <= 1) $ H.modify $ \s -> s { boosters = s.boosters - 1}
      pure next

    eval (SetRoomType t next) = do
      H.modify $ \st -> st { roomType = t, boosters = fst (packLimits t) }
      pure next

    eval (StartGame next) = do
      packs <- H.queryAll $ H.request GetBooster
      st <- H.get
      let packstr = intercalate ","<<<map setToLetters<<<M.values $ packs
          openurl = "/room/create/" <> case st.roomType of
            Draft -> packstr
            Sealed -> packstr <> "?sealed"
            BoosterBox -> packstr <> "?box"
      H.liftEff $ do
        win <- window
        open openurl "_self" "" win
      pure next

main :: forall e. Eff (HA.HalogenEffects (ajax :: AX.AJAX, console :: CONSOLE, window :: WINDOW | e)) Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  el <- HA.selectElement "#app"
  maybe (throwError (error "Could not find createroom")) (runUI mane unit) el

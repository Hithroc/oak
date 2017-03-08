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
import Halogen.Data.Prism (type (<\/>), type (\/))
import Halogen.Component.ChildPath as CP
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Data.Argonaut.Core as A
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


type ManeState
  = { boosterAmount :: Int
    , roomCount :: Maybe Int
    }

data ManeQuery a
  = AddBooster a
  | RemoveBooster a
  | StartGame a
  | Initialize a

type Slot = Int

type ManeAff eff = Aff (ajax :: AX.AJAX, console :: CONSOLE, window :: WINDOW, dom :: DOM | eff)
mane :: forall m. H.Component HH.HTML ManeQuery Unit Void (ManeAff m)
mane =
  H.lifecycleParentComponent
  { initialState : const { boosterAmount : 4, roomCount : Nothing }
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  , receiver : const Nothing
  }
  where
    render :: ManeState -> H.ParentHTML ManeQuery BoosterSelectorQuery Int (ManeAff m)
    render st =
      HH.div [ HP.class_ (ClassName "create-room-block")] $
      [ HH.div [HP.class_ (ClassName "roomcount")] [ HH.text $ "Amount of opened rooms: " <> maybe "0" show st.roomCount]
      , HH.button [ HE.onClick (HE.input_ StartGame) ] [ HH.text "Create Room" ]
      , HH.div [HP.class_ (ClassName "booster-buttons")]
        [ HH.button [ HP.disabled $ st.boosterAmount >= 8, HE.onClick (HE.input_ AddBooster) ] [ HH.text "Add pack" ]
        , HH.button [ HP.disabled $ st.boosterAmount <= 1, HE.onClick (HE.input_ RemoveBooster) ] [ HH.text "Remove pack" ]
        ]
      ]
      <> mapWithIndex (\i x -> HH.slot i x unit absurd) (replicate st.boosterAmount boosterSelector)

    eval :: ManeQuery ~> H.ParentDSL ManeState ManeQuery BoosterSelectorQuery Slot Void (ManeAff m)
    eval (Initialize next) = do
      response <- H.liftAff $ AX.get ("/rooms")
      H.modify $ _ { roomCount = fromString response.response }
      pure next

    eval (AddBooster next) = do
      st <- H.get
      unless (st.boosterAmount >= 8) $ H.modify $ _ { boosterAmount = st.boosterAmount + 1 }
      pure next

    eval (RemoveBooster next) = do
      st <- H.get
      unless (st.boosterAmount <= 1) $ H.modify $ _ { boosterAmount = st.boosterAmount - 1 }
      pure next

    eval (StartGame next) = do
      packs <- H.queryAll $ H.request GetBooster
      let packstr = intercalate ","<<<map setToLetters<<<M.values $ packs
      H.liftEff $ do
        win <- window
        open ("room/create/" <> packstr) "_self" "" win
      pure next

main :: forall e. Eff (HA.HalogenEffects (ajax :: AX.AJAX, console :: CONSOLE, window :: WINDOW | e)) Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  el <- HA.selectElement "#app"
  maybe (throwError (error "Could not find createroom")) (runUI mane unit) el

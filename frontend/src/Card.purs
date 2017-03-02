module Card where

import Prelude
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax as AX
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core(AttrName(..), ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Argonaut.Core as A
import Data.StrMap as M
import Data.String
import Data.Maybe
import Data.Tuple

type Card =
  { set :: String
  , number :: String
  }

jsonToCard :: A.Json -> Maybe Card
jsonToCard = A.foldJsonObject Nothing $ \jmap -> do
  set <- M.lookup "set" jmap >>= A.toString
  num <- M.lookup "num" jmap >>= A.toString
  pure { set: set, number: num }

-- Internal representation of Card
type CardState =
  { cid :: String
  , fallbacked :: Boolean
  }

data CardQuery a
  = Pick a
  | Fallback a

data CardMessage = Picked

getPonyHeadID :: Card -> Tuple String String
getPonyHeadID st = Tuple (getId toUpper) (getId toLower)
  where
    st' =
      if toLower st.set == "pr" && st.number == "213" -- Applejack Farm Foremare id fix. #typonyhead
      then { set: "pr", number: "PF2" }
      else st { number = replaceAll (Pattern "-") (Replacement "n") st.number }
    getId f =  toLower st'.set <> f (st'.number)


getImageURL :: String -> String
getImageURL str = base_url <> str <> ".jpg"
  where
    base_url = "http://ponyhead.com/img/cards/"

card :: forall m. Card -> H.Component HH.HTML CardQuery Unit CardMessage m
card c = 
  H.lifecycleComponent
    { initialState : const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    initialState :: CardState
    initialState = { cid: fst $ ponyHeadID, fallbacked: false }
    ponyHeadID = getPonyHeadID c
    render :: CardState -> H.ComponentHTML CardQuery
    render st =
        HH.img $
          [ HP.src $ getImageURL st.cid
          , HP.alt (show st.fallbacked)
          , HP.class_ (ClassName "card")
          , HE.onClick (HE.input_ Pick)
          , HP.attr (AttrName "style") "cursor: pointer"
          ] 
          <> if not st.fallbacked
          then [HE.onError (HE.input_ Fallback)]
          else []

    eval :: forall m. CardQuery ~> H.ComponentDSL CardState CardQuery CardMessage m
    eval =
      case _ of
        Pick next -> do
          H.raise Picked
          pure next
        Fallback next -> do
          H.modify $ _ { cid = snd ponyHeadID, fallbacked = true }
          pure next

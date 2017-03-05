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
  , ponyid :: Tuple String String
  , fallbacked :: Boolean
  }

data CardQuery a
  = Pick a
  | Fallback a
  | Update Card a
  | GetCID (String -> a)

data CardMessage
  = Picked
  | Updated

getPonyHeadID :: Card -> Tuple String String
getPonyHeadID st = Tuple (getId toUpper) (getId toLower)
  where
    st' =
      if toLower st.set == "pr" && st.number == "213" -- Applejack Farm Foremare id fix. #typonyhead
      then { set: "pr", number: "PF2" }
      else st
    -- We want to replace minus after toUpper, because n is always lowercased for negatives in PonyHead
    -- Everything would work anyway if we replaced '-' to 'n' before uppercasing,
    -- because the code will just fallback to lowercased version during onError handle,
    -- but it's better to avoid that.
    -- #typonyhead
    getId f =  toLower st'.set <> replaceMinus (f st'.number)
    replaceMinus = replaceAll (Pattern "-") (Replacement "n")


getImageURL :: String -> String
getImageURL str = base_url <> str <> ".jpg"
  where
    base_url = "http://ponyhead.com/img/cards/"

card :: forall m. H.Component HH.HTML CardQuery Card CardMessage m
card = 
  H.lifecycleComponent
    { initialState : initialState
    , render
    , eval
    , receiver: HE.input Update
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    initialState :: Card -> CardState
    initialState c = { cid: fst $ getPonyHeadID $ c , ponyid: getPonyHeadID $ c, fallbacked: false }
    render :: CardState -> H.ComponentHTML CardQuery
    render st =
        HH.div [ HP.class_ (ClassName "card"), HE.onClick (HE.input_ Pick) ]
        [ HH.div [ HP.class_ (ClassName "image-hover-hack") ] []
        , HH.img $
            [ HP.src $ getImageURL st.cid
            , HP.alt (show st.fallbacked)
            , HP.class_ (ClassName "card-image")
            ] 
          <> if not st.fallbacked
          then [HE.onError (HE.input_ Fallback)]
          else []
        ]

    eval :: forall m. CardQuery ~> H.ComponentDSL CardState CardQuery CardMessage m
    eval =
      case _ of
        Update c next -> do
          H.put $ initialState c
          pure next
        Pick next -> do
          H.raise Picked
          pure next
        Fallback next -> do
          H.modify $ \st -> st { cid = snd (st.ponyid), fallbacked = true }
          H.raise Updated
          pure next
        GetCID reply -> do
          st <- H.get
          pure (reply st.cid)

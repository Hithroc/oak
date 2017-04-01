module Card where

import Prelude
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
  , rarity :: String
  , smoozifying :: Boolean
  }

jsonToCard :: A.Json -> Maybe Card
jsonToCard = A.foldJsonObject Nothing $ \jmap -> do
  set <- M.lookup "set" jmap >>= A.toString
  num <- M.lookup "num" jmap >>= A.toString
  rarity <- M.lookup "rarity" jmap >>= A.toString
  pure { set: set, number: num, rarity: rarity, smoozifying: set == "mt" }

-- Internal representation of Card
type CardState =
  { cid :: String
  , ponyid :: Tuple String String
  , fallbacked :: Boolean
  , pickid :: Int
  , rarity :: String
  , smoozified :: Boolean
  , smoozifying :: Boolean
  }

data CardQuery a
  = Pick a
  | Fallback a
  | Update (Tuple Int Card) a
  | GetCID (String -> a)
  | Smoozify Boolean a

data CardMessage
  = Picked Int
  | Updated

getPonyHeadID :: Card -> Tuple String String
getPonyHeadID st = Tuple (getId toUpper) (getId toLower)
  where
    st' =
      if toLower st.set == "pr" && st.number == "213" -- Applejack Farm Foremare id fix. #typonyhead
      then st { set = "pr", number = "PF2" }
      else st
    -- We want to replace minus after toUpper, because n is always lowercased for negatives in PonyHead
    -- Everything would work anyway if we replaced '-' to 'n' before uppercasing,
    -- because the code will just fallback to lowercased version during onError handle,
    -- but it's better to avoid that.
    -- #typonyhead
    getId f =  toLower st'.set <> replaceMinus (f st'.number)
    replaceMinus = replaceAll (Pattern "-") (Replacement "n")


getImageURL :: CardState -> String
getImageURL st = base_url st.rarity <> st.cid <> ".jpg"
  where
    base_url "RR" = "http://hithroc.org/mlpccg/rrimg/"
    base_url _ = "http://ponyhead.com/img/cards/"

card :: forall m. H.Component HH.HTML CardQuery (Tuple Int Card) CardMessage m
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
    initialState :: Tuple Int Card -> CardState
    initialState c = { cid: fst $ getPonyHeadID $ snd c , ponyid: getPonyHeadID $ snd c, fallbacked: false, pickid: fst c, rarity: _.rarity $ snd c, smoozifying: _.smoozifying $ snd c, smoozified: true}
    render :: CardState -> H.ComponentHTML CardQuery
    render st =
        HH.div [ HP.class_ (ClassName "card"), HE.onClick (HE.input_ Pick) ]
        [ HH.div [ HP.class_ (ClassName "image-hover-hack"), HE.onMouseEnter (HE.input_ $ Smoozify false), HE.onMouseLeave (HE.input_ $ Smoozify true)] []
        , HH.img $
            [ HP.src $ if st.smoozified && st.smoozifying then "http://ponyhead.com/img/cards/mt74.jpg" else getImageURL st
            , HP.alt (show st.fallbacked)
            , HP.class_ (ClassName "card-image")
            ] 
          <> if not st.fallbacked
          then [HE.onError (HE.input_ Fallback)]
          else []
        ]

    eval :: CardQuery ~> H.ComponentDSL CardState CardQuery CardMessage m
    eval =
      case _ of
        Update c next -> do
          H.put $ initialState c
          pure next
        Pick next -> do
          st <- H.get
          H.raise $ Picked st.pickid
          pure next
        Fallback next -> do
          H.modify $ \st -> st { cid = snd (st.ponyid), fallbacked = true }
          H.raise Updated
          pure next
        GetCID reply -> do
          st <- H.get
          pure (reply st.cid)
        Smoozify x next -> do
          H.modify $ _ { smoozified = x }
          pure next

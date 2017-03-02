module CardList where

import Prelude
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax as AX
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core(AttrName(..), ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.String
import Data.Maybe
import Data.Tuple
import Data.Array (mapWithIndex)
import Data.Array as A
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.Console (log)


import Card as C

type CardListState = { counter :: Int, cards :: Array (Tuple Int C.Card), picked :: Boolean }

data CardListQuery a
  = PickCard Int a
  | NewCards (Array C.Card) Boolean a

data CardListMessage = CardPicked Int

chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n xs
  | A.null xs = []
  | otherwise = A.take n xs A.: chunksOf n (A.drop n xs)


type CardListAff eff = Aff (ajax :: AX.AJAX, console :: CONSOLE | eff)
cardList :: String -> Maybe String -> forall m. H.Component HH.HTML CardListQuery Unit CardListMessage (CardListAff m)
cardList name req =
  H.lifecycleParentComponent
  { initialState : const initialState
  , render
  , eval
  , receiver : const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }
  where
    initialState :: CardListState
    initialState = { counter: 0, cards : [], picked: false }
    render :: forall m. CardListState -> H.ParentHTML CardListQuery C.CardQuery Int (CardListAff m)
    render st =
      HH.div
        [ HP.class_ (ClassName "cardlist") 
        , HP.attr (AttrName "style") $ if st.picked then "filter:grayscale(100%)" else ""
        ]
        [ HH.div [HP.class_ (ClassName "cardlist-title")] [ HH.h1_ [HH.text name] ]
        , HH.div_<<<flip mapWithIndex st.cards $ \i c -> HH.slot (fst c) (C.card $ snd c) unit (listen i)
        ]

    eval :: forall m. CardListQuery ~> H.ParentDSL CardListState CardListQuery C.CardQuery Int CardListMessage (CardListAff m)
    eval =
      case _ of
        PickCard i next -> do
          case req of
            Just path -> do
              (response :: AX.AffjaxResponse Unit) <- H.liftAff $ AX.get (path <> show i)
              pure unit
            Nothing -> pure unit
          pure next
        NewCards newcards picked next -> do
          H.modify $ \st -> st { counter = (st.counter + A.length newcards), cards = mapWithIndex (\i -> Tuple (i+st.counter)) newcards, picked = picked }
          pure next

    listen :: Int -> C.CardMessage -> Maybe (CardListQuery Unit)
    listen i C.Picked = Just<<<H.action $ PickCard i

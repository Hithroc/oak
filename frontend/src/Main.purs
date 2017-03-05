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
import Data.StrMap as M
import Data.Maybe(Maybe(..))
import Data.Traversable(sequence)
import CardList (cardList, CardListQuery(..), CardListMessage(..))
import Card (Card(..), jsonToCard, card)
import PlayerList (playerList, PlayerListQuery(..))
import Data.Const
import Partial (crash)
import Network.HTTP.StatusCode


type ManeState = Unit

type CardListObj
  = { draft :: Array Card
    , pool :: Array Card
    , picked :: Boolean
    }

jsonToCardList :: A.Json -> Maybe CardListObj
jsonToCardList = A.foldJsonObject Nothing $ \jmap -> do
  jdraft <- M.lookup "draft" jmap >>= A.toArray
  jpool <- M.lookup "pool" jmap >>= A.toArray
  picked <- M.lookup "picked" jmap >>= A.toBoolean
  draft <- sequence $ map jsonToCard jdraft
  pool <- sequence $ map jsonToCard jpool
  pure { draft: draft, pool: pool, picked: picked }

data Event
  = PlayersUpdate
  | CardListUpdate

readEvent :: String -> Maybe Event
readEvent str
  | str == "PlayersUpdate" = Just PlayersUpdate
  | str == "CardListUpdate" = Just CardListUpdate
  | otherwise = Nothing

data ManeQuery a
  = EventLoop a
  | ProcessEvent Event a
  | Initialize a
  | StartGame a
  | SendPick Int a

-- This operator reminds me of train wagons
--           8
--          8
-- _____/o=I.<\/>.<\/>.<\/>.______   All aboard! Next station is CardListQuery!
-- ###############################
type ManeChildQuery = PlayerListQuery <\/> CardListQuery <\/> CardListQuery <\/> Const Void
type ManeChildSlot = Unit \/ Unit \/ Unit \/ Void

type ManeAff eff = Aff (ajax :: AX.AJAX, console :: CONSOLE | eff)
mane :: forall m. H.Component HH.HTML ManeQuery ManeState Void (ManeAff m)
mane =
  H.lifecycleParentComponent
  { initialState : const unit
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  , receiver : const Nothing
  }
  where
    render :: ManeState -> H.ParentHTML ManeQuery ManeChildQuery ManeChildSlot (ManeAff m)
    render st =
      HH.div [ HP.class_ (ClassName "mane")]
        [ HH.div [HP.class_ (ClassName "bar")] 
          [ HH.div [HP.class_ (ClassName "menu")]
            [ HH.button [HE.onClick (HE.input_ StartGame)] [HH.text "Start Game"]
            , HH.button [HE.onClick (HE.input_ StartGame)] [HH.text "Export to Ponyhead"]
            ]
          , HH.slot' CP.cp1 unit playerList unit absurd
          ]
        , HH.div [HP.class_ (ClassName "content")]
          [ HH.slot' CP.cp2 unit (cardList "Draft" $ Just "pick/") unit (const Nothing)
          , HH.slot' CP.cp3 unit (cardList "Pool" Nothing) unit (const Nothing)
          ]
        ]

    eval :: ManeQuery ~> H.ParentDSL ManeState ManeQuery ManeChildQuery ManeChildSlot Void (ManeAff m)
    eval (Initialize next)
      =  eval<<<ProcessEvent PlayersUpdate
     >=> eval<<<ProcessEvent CardListUpdate
     >=> eval<<<EventLoop $ next
    eval (EventLoop next) = do
      response <- H.liftAff $ AX.get "events"
      if response.status /= (StatusCode 200)
        then pure next
        else do
          next' <- case readEvent response.response of
            Nothing -> pure next
            Just e -> eval (ProcessEvent e next)
          eval (EventLoop next')
    eval (ProcessEvent event next) = do
      case event of
        PlayersUpdate -> do
          H.query' CP.cp1 unit (H.action UpdateUsernames)
          pure unit
        CardListUpdate -> do
          cardresp <- H.liftAff $ AX.get "cardlist"
          case jsonToCardList $ cardresp.response of
            Nothing -> pure unit
            Just cl -> do
              H.query' CP.cp2 unit (H.action (NewCards cl.draft cl.picked))
              H.query' CP.cp3 unit (H.action (NewCards cl.pool false))
              pure unit
      pure next
    eval (SendPick i next) = pure next
    eval (StartGame next) = do
      (response :: AX.AffjaxResponse Unit) <- H.liftAff $ AX.get ("start")
      pure next


main :: forall e. Eff (HA.HalogenEffects (ajax :: AJAX, console :: CONSOLE | e)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  --runUI (card { set: "cs", number: "F1" }) unit body
  runUI mane unit body

module Main where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff as AFF
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
import Data.Maybe(Maybe(..), maybe)
import Data.Traversable(sequence)
import CardList (cardList, CardListQuery(..), CardListMessage(..))
import Card (Card(..), jsonToCard, card)
import PlayerList (playerList, PlayerListQuery(..), jsonToPlayers)
import Data.Const
import Network.HTTP.StatusCode(StatusCode(..))
import DOM.HTML.Types(WINDOW, ALERT)
import DOM (DOM)
import DOM.HTML(window)
import DOM.HTML.Window(open, alert)
import DOM.HTML.Window as W
import DOM.HTML.Location as L
import Control.Monad.Aff.AVar
import Control.Monad.Eff.Ref (REF)
import WebSocket
import Control.Monad.Eff.Var (($=), get)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (forever)
import Data.String (replaceAll, Pattern(..), Replacement(..))
import Data.Argonaut.Core as A
import Data.Argonaut.Parser as A
import Data.Tuple
import Data.Either
import Data.Foreign (toForeign, unsafeFromForeign)
import Control.Monad.Except (runExcept)
import Data.Foreign.Index (prop)


type ManeState
  = { banner :: Maybe String
    }

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

jsonPayload :: A.Json -> Maybe (Tuple String A.Json)
jsonPayload = A.foldJsonObject Nothing $ \jmap -> do
  name <- M.lookup "name" jmap >>= A.toString
  data_ <- M.lookup "data" jmap
  pure $ Tuple name data_

data Event
  = PlayersUpdate A.Json
  | CardListUpdate A.Json

data ManeQuery a
  = EventLoop a
  | ProcessEvent Event a
  | Initialize a
  | StartGame a
  | OpenDeckURL a

-- This operator reminds me of train wagons
--           8
--          8
-- _____/o=I.<\/>.<\/>.<\/>.______   All aboard! Next station is CardListQuery!
-- ###############################
type ManeChildQuery = PlayerListQuery <\/> CardListQuery <\/> CardListQuery <\/> Const Void
type ManeChildSlot = Unit \/ Unit \/ Unit \/ Void

type Effects e = (ajax :: AX.AJAX, console :: CONSOLE, window :: WINDOW, alert :: ALERT, ws :: WEBSOCKET| e)
type Effects2 e = Effects (dom :: DOM, ref :: REF, avar :: AVAR, err :: EXCEPTION | e)
type ManeAff eff = Aff (Effects2 eff)
mane :: forall m. H.Component HH.HTML ManeQuery Unit Void (ManeAff m)
mane =
  H.lifecycleParentComponent
  { initialState : const { banner : Nothing }
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  , receiver : const Nothing
  }
  where
    render :: ManeState -> H.ParentHTML ManeQuery ManeChildQuery ManeChildSlot (ManeAff m)
    render st =
      HH.div [ HP.class_ (ClassName "mane")] $
        (maybe [] (\x -> [ HH.div [HP.class_ (ClassName "banner")] [ HH.text x ] ]) st.banner) <>
        [ HH.div [HP.class_ (ClassName "bar")] 
          [ HH.slot' CP.cp1 unit playerList unit absurd
          , HH.div [HP.class_ (ClassName "menu")]
            [ HH.button [HE.onClick (HE.input_ StartGame)] [HH.text "Start game"]
            , HH.button [HE.onClick (HE.input_ OpenDeckURL)] [HH.text "Export deck"]
            ]
          ]
        , HH.div [HP.class_ (ClassName "content")]
          [ HH.div [ HP.class_ (ClassName "content-container") ]
            [ HH.slot' CP.cp2 unit (cardList "Draft" $ Just "pick/") unit (const Nothing)
            , HH.slot' CP.cp3 unit (cardList "Pool" Nothing) unit (const Nothing)
            ]
          ]
        ]

    eval :: ManeQuery ~> H.ParentDSL ManeState ManeQuery ManeChildQuery ManeChildSlot Void (ManeAff m)
    eval (Initialize next) = eval (EventLoop next)
    eval (EventLoop next) = do
      win <- H.liftEff $ window
      loc <- H.liftEff $ W.location win
      locHref <- H.liftEff $ L.href loc
      let
        alert' = flip alert win
        wsurl = replaceAll (Pattern "https://") (Replacement "wss://") 
             <<<replaceAll (Pattern "http://") (Replacement "ws://")
              $ locHref
      H.liftAff $ log wsurl
      avar <- H.liftAff makeVar
      Connection socket <- H.liftEff $ newWebSocket (URL $ wsurl <> "events") []
      
      H.liftEff $ socket.onmessage $= \event -> do
        launchAff $ do
          let received = runMessage (runMessageEvent event)
          log $ "onmessage: Received '" <> received <> "'"
          putVar avar received
        pure unit
      let
        runCloseEvent event = case runExcept (prop "reason" (toForeign event)) of
          Right x -> unsafeFromForeign x
          Left _ -> ""
      H.liftEff $ socket.onclose $= \event -> alert' $ "Connection was closed: " <> runCloseEvent event
      H.liftEff $ socket.onerror $= \event -> alert' $ "Connection with the server was lost"
      forever $ do
        e <- H.liftAff $ takeVar avar
        H.liftAff $ log e
        let
          eitherToMaybe (Right x) = Just x
          eitherToMaybe _ = Nothing
        case eitherToMaybe (A.jsonParser e) >>= jsonPayload of
          Nothing -> pure next
          Just payload -> do
            case fst payload of
              "player_list" -> eval (ProcessEvent (PlayersUpdate (snd payload)) next)
              "card_list" -> eval (ProcessEvent (CardListUpdate (snd payload)) next)
              "shutdown" -> do
                H.liftEff $ socket.onclose $= \event -> pure unit
                H.liftEff $ socket.onerror $= \event -> pure unit
                H.liftEff $ socket.close
                H.modify $ _ { banner = Just "Server is shutting down. Please wait 10 seconds until it restarts. The page will be reloaded automatically." }
                H.liftAff $ AFF.later' 10000 (log "Test" >>= \_ -> void $ AFF.liftEff' $ L.reload loc)
                pure next
              _ -> pure next
      pure next

    eval (ProcessEvent event next) = do
      case event of
        PlayersUpdate pldata -> do
          case jsonToPlayers pldata of
            Nothing -> pure unit
            Just players -> do
              H.query' CP.cp1 unit (H.action $ UpdateUsernames players)
              pure unit
        CardListUpdate pldata -> do
          case jsonToCardList pldata of
            Nothing -> pure unit
            Just cl -> do
              H.query' CP.cp2 unit (H.action (NewCards cl.draft cl.picked))
              H.query' CP.cp3 unit (H.action (NewCards cl.pool false))
              pure unit
      pure next
    eval (StartGame next) = do
      (response :: AX.AffjaxResponse Unit) <- H.liftAff $ AX.get ("start")
      pure next
    eval (OpenDeckURL next) = do
      deckurl <- H.query' CP.cp3 unit (H.request GetDeckURL)
      H.liftEff $ do
        win <- window
        open (maybe "" id deckurl) "" "" win
      pure next


main :: forall e. Eff (HA.HalogenEffects (Effects e)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  --runUI (card { set: "cs", number: "F1" }) unit body
  runUI mane unit body

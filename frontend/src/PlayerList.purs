module PlayerList (playerList, PlayerListQuery(..)) where

import Prelude
import Control.Monad.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Data.StrMap as M
import Data.Argonaut.Core as A
import Data.Maybe(Maybe(..))
import Halogen.HTML.Core(AttrName(..), ClassName(..))
import Data.Traversable(sequence)

type Player =
  { name :: String
  , picked :: Boolean
  }

jsonToPlayer :: A.Json -> Maybe Player
jsonToPlayer = A.foldJsonObject Nothing $ \jmap -> do
  name <- M.lookup "name" jmap >>= A.toString
  picked <- M.lookup "picked" jmap >>= A.toBoolean
  pure $ { name: name, picked: picked }

jsonToPlayers :: A.Json -> Maybe (Array Player)
jsonToPlayers = A.foldJsonArray Nothing (sequence<<<map jsonToPlayer)

type PlayerListState =
  { players :: Array Player
  , inputname :: String
  }

data PlayerListQuery a
  = UpdateUsernames a
  | ChangeInputname String a
  | MakeNamechangeRequest a

type PlayerListAff eff = Aff (ajax :: AX.AJAX | eff)

playerList :: forall eff. H.Component HH.HTML PlayerListQuery Unit Void (PlayerListAff eff)
playerList =
  H.lifecycleComponent
    { initialState : const initialState
    , render
    , eval
    , initializer: Just (H.action UpdateUsernames)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where
    initialState :: PlayerListState
    initialState = { players: [], inputname: "" }

    render :: PlayerListState -> H.ComponentHTML PlayerListQuery
    render st =
      HH.div_ $
        [  HH.ul_ $ map (\x -> HH.li [HP.class_ (ClassName $ if x.picked then "player-picked" else "player")] [HH.text x.name]) st.players
        ,  HH.div_
          [  HH.input [ HP.value st.inputname, HE.onValueInput (HE.input ChangeInputname)]
          ,  HH.button [HE.onClick (HE.input_ MakeNamechangeRequest)] [HH.text "Change"]
          ]
        ]

    eval :: PlayerListQuery ~> H.ComponentDSL PlayerListState PlayerListQuery Void (PlayerListAff eff)
    eval =
      case _ of
        ChangeInputname str next -> do
          H.modify (_ { inputname = str })
          pure next
        UpdateUsernames next -> do
          response <- H.liftAff $ AX.get "playerlist"
          case jsonToPlayers response.response of
            Nothing -> pure unit
            Just p -> void $ H.modify (_ { players = p })
          pure next
        MakeNamechangeRequest next -> do
          n <- H.gets _.inputname
          H.liftAff $ AX.post_ "changename" n
          H.modify (_ { inputname = "" })
          pure next

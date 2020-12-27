module Main where

import Prelude

import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Elmish (ComponentDef, DispatchMsgFn, ReactElement, Transition, bimap, lmap, (>#<))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Footer as Footer
import Header as Header
import Home as Home

main :: Effect Unit
main =
  defaultMain
    { elementId: "app"
    , def
    }

type State =
  { home :: Home.State
  }

data Message
  = HomeMsg Home.Message

def :: forall m. MonadAff m => ComponentDef m Message State
def =
  { init, update, view }
  where
    init :: Transition m Message State
    init = do
      home <- Home.init # lmap HomeMsg
      pure { home }

    update :: State -> Message -> Transition m Message State
    update state = case _ of
      HomeMsg msg ->
        Home.update state.home msg
        # bimap HomeMsg state { home = _ }

    view :: State -> DispatchMsgFn Message -> ReactElement
    view state dispatch = H.fragment
      [ Header.view
      , Home.view state.home (dispatch >#< HomeMsg)
      , Footer.view
      ]

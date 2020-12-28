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

data State
  = Home Home.State

data Message
  = HomeMsg Home.Message

def :: forall m. MonadAff m => ComponentDef m Message State
def =
  { init, update, view }
  where
    init :: Transition m Message State
    init = do
      home <- Home.init # lmap HomeMsg
      pure $ Home home

    update :: State -> Message -> Transition m Message State
    update state message = case message, state of
      HomeMsg msg, Home home ->
        Home.update home msg
        # bimap HomeMsg Home

    view :: State -> DispatchMsgFn Message -> ReactElement
    view state dispatch = H.fragment
      [ Header.view
      , body
      , Footer.view
      ]
      where
        body = case state of
          Home home -> Home.view home (dispatch >#< HomeMsg)

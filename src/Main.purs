module Main where

import Prelude

import Article as Article
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Elmish (ComponentDef, DispatchMsgFn, ReactElement, Transition, bimap, forkVoid, lmap, (>#<))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Footer as Footer
import Header as Header
import Home as Home
import Web.HTML (window)
import Web.HTML.Location (hash)
import Web.HTML.Window (location)

main :: Effect Unit
main =
  defaultMain
    { elementId: "app"
    , def
    }

data State
  = Article Article.State
  | Home Home.State

data Message
  = ArticleMsg Article.Message
  | HomeMsg Home.Message

def :: forall m. MonadAff m => ComponentDef m Message State
def =
  { init, update, view }
  where
    init :: Transition m Message State
    init = do
      forkVoid $ liftEffect do
        route <- window >>= location >>= hash
        pure unit
      home <- Home.init # lmap HomeMsg
      pure $ Home home
      -- article <- Article.init "g-rspj2w" # lmap ArticleMsg
      -- pure $ Article article

    update :: State -> Message -> Transition m Message State
    update state message = case message, state of
      HomeMsg msg, Home home ->
        Home.update home msg
        # bimap HomeMsg Home
      HomeMsg msg, _ ->
        pure state
      ArticleMsg msg, Article article ->
        Article.update article msg
        # bimap ArticleMsg Article
      ArticleMsg msg, _ ->
        pure state

    view :: State -> DispatchMsgFn Message -> ReactElement
    view state dispatch = H.fragment
      [ Header.view
      , body
      , Footer.view
      ]
      where
        body = case state of
          Home home -> Home.view home (dispatch >#< HomeMsg)
          Article article -> Article.view article (dispatch >#< ArticleMsg)

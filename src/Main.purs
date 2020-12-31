module Main where

import Prelude

import Article as Article
import Data.Maybe (maybe)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Elmish (ComponentDef, DispatchMsgFn, ReactElement, Transition, bimap, forkMaybe, forkVoid, forks, lmap, (>#<))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Footer as Footer
import Foreign (unsafeToForeign)
import Header as Header
import Home as Home
import Router (Route)
import Router as Router
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.History as History
import Web.HTML.Location (hash)
import Web.HTML.Window (history, location)
import Web.HTML.Window as Window

main :: Effect Unit
main =
  defaultMain
    { elementId: "app"
    , def
    }

data State
  = Article Article.State
  | Home Home.State
  | Loading

data Message
  = ArticleMsg Article.Message
  | HomeMsg Home.Message
  | SetRoute Route

def :: forall m. MonadAff m => ComponentDef m Message State
def =
  { init, update, view }
  where
    init :: Transition m Message State
    init = do
      forkMaybe $ liftEffect do
        hash' <- window >>= location >>= hash
        pure $ SetRoute <$> Router.parse hash'
      forks \dispatch -> liftEffect do
        window' <- Window.toEventTarget <$> window
        handleRouteChange <- eventListener \_ -> do
          hash' <- window >>= location >>= hash
          maybe (pure unit) dispatch $ SetRoute <$> Router.parse hash'
        addEventListener (EventType "hashchange") handleRouteChange false window'
      pure Loading

    update :: State -> Message -> Transition m Message State
    update state message = case message, state of
      ArticleMsg msg, Article article ->
        Article.update article msg
        # bimap ArticleMsg Article
      ArticleMsg msg, _ ->
        pure state
      HomeMsg msg, Home home ->
        Home.update home msg
        # bimap HomeMsg Home
      HomeMsg msg, _ ->
        pure state
      SetRoute route, _ -> do
        forkVoid $ liftEffect do
          let newHash = Router.print route
          currentHash <- window >>= location >>= hash
          when (currentHash /= newHash) $
            window >>= history >>= History.pushState
              (unsafeToForeign Nullable.null)
              (History.DocumentTitle "Conduit")
              (History.URL newHash)
        case route of
          Router.Home -> do
            home <- Home.init # lmap HomeMsg
            pure $ Home home
          Router.Article slug -> do
            article <- Article.init slug # lmap ArticleMsg
            pure $ Article article

    view :: State -> DispatchMsgFn Message -> ReactElement
    view state dispatch = H.fragment
      [ Header.view
      , body
      , Footer.view
      ]
      where
        body = case state of
          Article article ->
            Article.view article (dispatch >#< ArticleMsg)
          Home home ->
            Home.view home (dispatch >#< HomeMsg)
          Loading ->
            H.empty

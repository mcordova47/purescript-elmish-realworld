module Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Elmish (ComponentDef, DispatchMsgFn, ReactElement, Transition, bimap, forkMaybe, forkVoid, forks, lmap, (>#<))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Foreign (unsafeToForeign)
import Layout.Footer as Footer
import Layout.Header as Header
import Pages.Article as Article
import Pages.Home as Home
import Pages.Login as Login
import Router (Route)
import Router as Router
import Types.Article as Types
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
  | Login Login.State
  | Loading

data Message
  = ArticleMsg Article.Message
  | HomeMsg Home.Message
  | LoginMsg Login.Message
  | SetRoute Route

def :: forall m. MonadAff m => ComponentDef m Message State
def =
  { init, update, view }

init :: forall m. MonadAff m => Transition m Message State
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

update :: forall m. MonadAff m => State -> Message -> Transition m Message State
update state message = case message, state of
  ArticleMsg msg, Article article ->
    Article.update article msg
    # bimap ArticleMsg Article
  ArticleMsg msg, _ ->
    pure state
  HomeMsg msg@(Left _), Home home ->
    Home.update home msg
    # bimap HomeMsg Home
  HomeMsg msg@(Left _), _ ->
    pure state
  HomeMsg (Right (Home.SelectArticle article@(Types.Article { slug }))), Home home -> do
    let route = Router.Article slug
    ensureCorrectUrl route
    articleState <- Article.init { article: Just article, slug } # lmap ArticleMsg
    pure $ Article articleState
  HomeMsg (Right _), _ ->
    pure state
  LoginMsg msg, Login login ->
    Login.update login msg
    # bimap LoginMsg Login
  LoginMsg msg, _ ->
    pure state
  SetRoute route, _ -> do
    ensureCorrectUrl route
    case route of
      Router.Home -> do
        home <- Home.init # lmap HomeMsg
        pure $ Home home
      Router.Article slug -> do
        article <- Article.init { article: Nothing, slug } # lmap ArticleMsg
        pure $ Article article
      Router.Login -> do
        login <- Login.init Login.LoginPage # lmap LoginMsg
        pure $ Login login
      Router.Register -> do
        login <- Login.init Login.RegisterPage # lmap LoginMsg
        pure $ Login login

view :: State -> DispatchMsgFn Message -> ReactElement
view state dispatch = H.fragment
  [ Header.view currentRoute
  , body
  , Footer.view
  ]
  where
    body = case state of
      Article article ->
        Article.view article (dispatch >#< ArticleMsg)
      Home home ->
        Home.view home (dispatch >#< HomeMsg)
      Login login ->
        Login.view login (dispatch >#< LoginMsg)
      Loading ->
        H.empty

    currentRoute = case state of
      Article { article: Just (Types.Article { slug }) } -> Router.Article slug
      Home _ -> Router.Home
      Login (Login.Login _) -> Router.Login
      Login (Login.Register _) -> Router.Register
      _ -> Router.Home

ensureCorrectUrl :: forall m. MonadEffect m => Route -> Transition m Message Unit
ensureCorrectUrl route = forkVoid $ liftEffect do
  let correctHash = Router.print route
  currentHash <- window >>= location >>= hash
  when (currentHash /= correctHash) $
    window >>= history >>= History.pushState
      (unsafeToForeign Nullable.null)
      (History.DocumentTitle "Conduit")
      (History.URL correctHash)

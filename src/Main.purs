module Main where

import Prelude

import Api (ArticlesResponse(..))
import Api as Api
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Elmish (ComponentDef, DispatchMsgFn, ReactElement, Transition, forkMaybe)
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Footer as Footer
import Header as Header
import Home as Home
import Types.Article (Article)

main :: Effect Unit
main =
  defaultMain
    { elementId: "app"
    , def
    }

type State =
  { articles :: Array Article
  }

data Message
  = SetArticles (Array Article)

def :: forall m. MonadAff m => ComponentDef m Message State
def =
  { init, update, view }
  where
    init :: Transition m Message State
    init = do
      forkMaybe do
        resp <- Api.articles
        case resp of
          Right (ArticlesResponse articlesResponse) ->
            pure $ Just $ SetArticles articlesResponse.articles
          Left err ->
            pure Nothing
      pure { articles: [] }

    update :: State -> Message -> Transition m Message State
    update state = case _ of
      SetArticles articles ->
        pure state { articles = articles }

    view :: State -> DispatchMsgFn Message -> ReactElement
    view state _ = H.fragment
      [ Header.view
      , Home.view state
      , Footer.view
      ]

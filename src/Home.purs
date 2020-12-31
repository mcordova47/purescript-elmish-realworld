module Home
  ( Message
  , State
  , init
  , update
  , view
  ) where

import Prelude

import Api (ArticlesResponse(..), TagsResponse(..))
import Api as Api
import Data.Array ((..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Effect.Aff.Class (class MonadAff)
import Elmish (DispatchMsgFn, ReactElement, Transition, forkMaybe, (>#<))
import Elmish.HTML.Styled as H
import Elmish.React (class ReactChildren)
import Router as Router
import Types.Article (Article(..))
import Types.Author (Author(..))
import Utils.DateTime as DateTime
import Utils.EventHandler as EventHandler

type State =
  { articles :: Array Article
  , page :: Int
  , selectedTag :: Maybe String
  , tags :: Array String
  , totalPages :: Int
  }

data Message
  = SetArticles ArticlesResponse
  | SelectPage Int
  | SelectTag String
  | SetTags (Array String)

init :: forall m. MonadAff m => Transition m Message State
init = do
  let
    initialState =
      { articles: []
      , page: 1
      , selectedTag: Nothing
      , tags: []
      , totalPages: 0
      }
  fetchArticles initialState
  forkMaybe do
    resp <- Api.tags
    case resp of
      Right (TagsResponse tagsResponse) ->
        pure $ Just $ SetTags tagsResponse.tags
      Left err ->
        pure Nothing
  pure initialState

update :: forall m. MonadAff m => State -> Message -> Transition m Message State
update state = case _ of
  SetArticles (ArticlesResponse articlesResponse) ->
    pure state
      { articles = articlesResponse.articles
      , totalPages = articlesResponse.articlesCount / pageLimit
      }
  SelectPage page -> do
    let state' = state { page = page }
    fetchArticles state'
    pure state'
  SelectTag tag -> do
    let state' = state { selectedTag = Just tag }
    fetchArticles state'
    pure state'
  SetTags tags ->
    pure state { tags = tags }

view :: State -> DispatchMsgFn Message -> ReactElement
view state dispatch =
  H.div "home-page"
  [ H.div "banner" $
    H.div "container" $
      [ H.h1 "logo-font" "conduit"
      , H.p "" "A place to share your knowledge."
      ]
  , H.div "container page" $
      H.div "row"
      [ H.div "col-md-9"
        [ H.div "feed-toggle" $
            H.ul "nav nav-pills outline-active"
            [ tab { active: false, disabled: true, label: "Your Feed" }
            , tab { active: isNothing state.selectedTag, disabled: false, label: "Global Feed" }
            , case state.selectedTag of
                Just tag -> tab
                  { active: isJust state.selectedTag
                  , disabled: false
                  , label:
                      [ H.i "ion-pound" H.empty
                      , H.text $ " " <> tag
                      ]
                  }
                Nothing -> H.empty
            ]
        , H.fragment $ articlePreview <$> state.articles
        , H.nav "" $
            H.ul "pagination" $
              pageLink <$> allPages
        ]
      , H.div "col-md-3" $
          H.div "sidebar"
          [ H.p "" "Popular Tags"
          , H.div "tag-list" $
              tagLink <$> state.tags
          ]
      ]
  ]
  where
    tagLink tag =
      H.a_ "tag-pill tag-default"
        { href: ""
        , onClick: EventHandler.withEvent $
            EventHandler.withPreventDefault $ dispatch >#< const (SelectTag tag)
        }
        tag

    pageLink page =
      H.li ("page-item" <> if page == state.page then " active" else "") $
        H.a_ "page-link"
          { href: ""
          , onClick: EventHandler.withEvent $
              EventHandler.withPreventDefault $ dispatch >#< const (SelectPage page)
          } $
          show page

    allPages = case state.totalPages of
      0 -> []
      _ -> 1..state.totalPages

tab :: forall content. ReactChildren content => { active :: Boolean, disabled :: Boolean, label :: content } -> ReactElement
tab { active, disabled, label } =
  H.li "nav-item" $
    H.a_ ("nav-link" <> if active then " active" else "" <> if disabled then " disabled" else "")
      { href: "" }
      label

articlePreview :: Article -> ReactElement
articlePreview (Article article) =
  H.div "article-preview"
  [ H.div "article-meta"
    [ H.a_ "" { href: "profile.html" } $
        H.img_ "" { src: author.image }
    , H.div "info" $
      [ H.a_ "author" { href: "" } author.username
      , H.span "date" $ DateTime.formatAsDate article.createdAt
      ]
    , H.button "btn btn-outline-primary btn-sm pull-xs-right"
      [ H.i "ion-heart" H.empty
      , H.text $ " " <> show article.favoritesCount
      ]
    ]
  , H.a_ "preview-link" { href: Router.print $ Router.Article article.slug }
    [ H.h1 "" article.title
    , H.p "" article.description
    , H.span "" "Read more..."
    , H.ul "tag-list" $
        H.li "tag-default tag-pill tag-outline" <$> article.tagList
    ]
  ]
  where
    Author author = article.author

fetchArticles :: forall m. MonadAff m => State -> Transition m Message Unit
fetchArticles state = forkMaybe do
  resp <- Api.articles
    { limit: Just pageLimit
    , offset: Just $ (state.page - 1) * pageLimit
    , tag: state.selectedTag
    }
  case resp of
    Right articlesResponse ->
      pure $ Just $ SetArticles articlesResponse
    Left err ->
      pure Nothing

pageLimit :: Int
pageLimit = 10

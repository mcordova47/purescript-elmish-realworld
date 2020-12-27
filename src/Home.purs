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
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Effect.Aff.Class (class MonadAff)
import Elmish (DispatchMsgFn, ReactElement, Transition, forkMaybe)
import Elmish.HTML.Styled as H
import Types.Article (Article(..))
import Types.Author (Author(..))
import Utils.DateTime as DateTime

type State =
  { articles :: Array Article
  , selectedTag :: Maybe String
  , tags :: Array String
  }

data Message
  = SetArticles (Array Article)
  | SetTags (Array String)

init :: forall m. MonadAff m => Transition m Message State
init = do
  forkMaybe do
    resp <- Api.articles
    case resp of
      Right (ArticlesResponse articlesResponse) ->
        pure $ Just $ SetArticles articlesResponse.articles
      Left err ->
        pure Nothing
  forkMaybe do
    resp <- Api.tags
    case resp of
      Right (TagsResponse tagsResponse) ->
        pure $ Just $ SetTags tagsResponse.tags
      Left err ->
        pure Nothing
  pure { articles: [], tags: [], selectedTag: Nothing }

update :: forall m. Monad m => State -> Message -> Transition m Message State
update state = case _ of
  SetArticles articles ->
    pure state { articles = articles }
  SetTags tags ->
    pure state { tags = tags }

view :: State -> DispatchMsgFn Message -> ReactElement
view props dispatch =
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
            , tab { active: isNothing props.selectedTag, disabled: false, label: "Global Feed" }
            , case props.selectedTag of
                Just tag -> tab { active: false, disabled: true, label: tag }
                Nothing -> H.empty
            ]
        , H.fragment $ articlePreview <$> props.articles
        ]
      , H.div "col-md-3" $
          H.div "sidebar"
          [ H.p "" "Popular Tags"
          , H.div "tag-list" $
            H.a_ "tag-pill tag-default" { href: "" } <$> props.tags
          ]
      ]
  ]

tab :: { active :: Boolean, disabled :: Boolean, label :: String } -> ReactElement
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
  , H.a_ "preview-link" { href: "" }
    [ H.h1 "" article.title
    , H.p "" article.description
    , H.span "" "Read more..."
    , H.ul "tag-list" $
        H.li "tag-default tag-pill tag-outline" <$> article.tagList
    ]
  ]
  where
    Author author = article.author

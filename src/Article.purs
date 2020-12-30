module Article
  ( Message
  , State
  , init
  , update
  , view
  ) where

import Prelude

import Api as Api
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Elmish (DispatchMsgFn, ReactElement, Transition, forkMaybe)
import Elmish.HTML.Styled as H
import Types.Article (Article(..))
import Types.Author (Author(..))
import Utils.DateTime as DateTime
import Utils.Html as Html

type State =
  { article :: Maybe Article
  }

data Message
  = SetArticle Article

init :: forall m. MonadAff m => String -> Transition m Message State
init slug = do
  forkMaybe do
    resp <- Api.article slug
    case resp of
      Right (Api.ArticleResponse { article }) ->
        pure $ Just $ SetArticle article
      Left err ->
        pure Nothing
  pure { article: Nothing }

update :: forall m. Monad m => State -> Message -> Transition m Message State
update state = case _ of
  SetArticle article ->
    pure state { article = Just article }

view :: State -> DispatchMsgFn Message -> ReactElement
view state _ = case state.article of
  Just (Article article@{ author: Author author}) ->
    H.div "article-page"
    [ H.div "banner" $
        H.div "container"
        [ H.h1 "" article.title
        , H.div "article-meta"
          [ H.a_ "" { href: "" } $
              H.img_ "" { src: author.image }
          , H.div "info"
            [ H.a_ "author" { href: "" } author.username
            , H.span "date" $ DateTime.formatAsDate article.createdAt
            ]
          , H.button "btn btn-sm btn-outline-secondary"
            [ H.i "ion-plus-round" H.empty
            , Html.nbsp
            , H.text $ "Follow " <> author.username
            ]
          , Html.nbsp
          , Html.nbsp
          , H.button "btn btn-sm btn-outline-primary"
            [ H.i "ion-heart" H.empty
            , Html.nbsp
            , H.text "Favorite Post"
            , H.span "counter" $
                " (" <> show article.favoritesCount <> ")"
            ]
          ]
        ]
    , H.div "container page"
      [ H.div "row article-content" $
          H.div "col-md-12" $
            -- TODO: Formatting
            H.p "" article.body
      ]
    , H.hr ""
    , H.div "article-actions" $
        H.div "article-meta"
        [ H.a_ "" { href: "profile.html" } $
            H.img_ "" { src: "http://i.imgur.com/Qr71crq.jpg" }
        , H.div "info"
          [ H.a_ "author" { href: "" } "Eric Simons"
          , H.span "date" "January 20th"
          ]
        , H.button "btn btn-sm btn-outline-secondary"
          [ H.i "ion-plus-round" H.empty
          , Html.nbsp
          , H.text "Follow Eric Simons"
          , H.span "counter" "(10)"
          ]
        , Html.nbsp
        , H.button "btn btn-sm btn-outline-primary"
          [ H.i "ion-heart" H.empty
          , Html.nbsp
          , H.text "Favorite Post"
          , H.span "counter" "(29)"
          ]
        ]
    , H.div "row" $
        H.div "col-xs-12 col-md-8 offset-md-2"
        [ H.form "card comment-form" $
          [ H.div "card-block" $
              H.textarea_ "form-control"
                { placeholder: "Write a comment..."
                , rows: 3
                , value: ""
                }
                H.empty
          , H.div "card-footer"
            [ H.img_ "comment-author-img" { src: "http://i.imgur.com/Qr71crq.jpg" }
            , H.button "btn btn-sm btn-primary" "Post Comment"
            ]
          ]
        , H.div "card"
          [ H.div "card-block" $
              H.p "card-text" "With supporting text below as a natural lead-in to additional content."
          , H.div "card-footer"
            [ H.a_ "comment-author" { href: "" } $
                H.img_ "comment-author-img" { src: "http://i.imgur.com/Qr71crq.jpg" }
            , Html.nbsp
            , H.a_ "comment-author" { href: "" } "Jacob Schmidt"
            , H.span "date-posted" "Dec 29th"
            ]
          ]
        , H.div "card" $
          [ H.div "card-block" $
              H.p "card-text" "With supporting text below as a natural lead-in to additional content."
          , H.div "card-footer"
            [ H.a_ "comment-author" { href: "" } $
                H.img_ "comment-author-img" { src: "http://i.imgur.com/Qr71crq.jpg" }
            , Html.nbsp
            , H.a_ "comment-author" { href: "" } "Jacob Schmidt"
            , H.span "date-posted" "Dec 29th"
            , H.span "mod-options"
              [ H.i "ion-edit" H.empty
              , H.i "ion-trash-a" H.empty
              ]
            ]
          ]
        ]
    ]
  Nothing ->
    H.empty

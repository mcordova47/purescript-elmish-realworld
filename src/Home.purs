module Home
  ( view
  ) where

import Prelude

import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Types.Article (Article(..))
import Types.Author (Author(..))
import Utils.DateTime as DateTime

type Props r =
  { articles :: Array Article
  | r
  }

view :: forall r. Props r -> ReactElement
view props =
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
            [ H.li "nav-item" $
                H.a_ "nav-link disabled" { href: "" } "Your Feed"
            , H.li "nav-item" $
                H.a_ "nav-link active" { href: "" } "Global Feed"
            ]
        , H.fragment $ articlePreview <$> props.articles
        ]
      , H.div "col-md-3" $
          H.div "sidebar"
          [ H.p "" "Popular Tags"
          , H.div "tag-list"
            [ H.a_ "tag-pill tag-default" { href: "" } "programming"
            , H.a_ "tag-pill tag-default" { href: "" } "javascript"
            , H.a_ "tag-pill tag-default" { href: "" } "emberjs"
            , H.a_ "tag-pill tag-default" { href: "" } "angularjs"
            , H.a_ "tag-pill tag-default" { href: "" } "react"
            , H.a_ "tag-pill tag-default" { href: "" } "mean"
            , H.a_ "tag-pill tag-default" { href: "" } "node"
            , H.a_ "tag-pill tag-default" { href: "" } "rails"
            ]
          ]
      ]
  ]

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
    , H.p "" article.body
    , H.span "" "Read more..."
    ]
  ]
  where
    Author author = article.author

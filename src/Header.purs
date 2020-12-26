module Header
  ( view
  ) where

import Prelude

import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Utils.Html as Html

view :: ReactElement
view =
  H.nav "navbar navbar-light" $
    H.div "container"
    [ H.a_ "navbar-brand" { href: "index.html" } "conduit"
    , H.ul "nav navbar-nav pull-xs-right" $
      [ H.li "nav-item" $
          H.a_ "nav-link active" { href: "" } "Home"
      , H.li "nav-item" $
          H.a_ "nav-link" { href: "" }
            [ H.i "ion-compose" H.empty
            , Html.nbsp
            , H.text "New Post"
            ]
      , H.li "nav-item" $
          H.a_ "nav-link" { href: "" }
            [ H.i "ion-gear-a" H.empty
            , Html.nbsp
            , H.text "Settings"
            ]
      , H.li "nav-item" $
          H.a_ "nav-link" { href: "" }
            [ H.i "ion-gear-a" H.empty
            , Html.nbsp
            , H.text "Sign up"
            ]
      ]
    ]

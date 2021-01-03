module Layout.Header
  ( view
  ) where

import Prelude

import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Router as Router
import Utils.Html as Html

view :: ReactElement
view =
  H.nav "navbar navbar-light" $
    H.div "container"
    [ H.a_ "navbar-brand" { href: Router.print Router.Home } "conduit"
    , H.ul "nav navbar-nav pull-xs-right" $
      [ H.li "nav-item" $
          H.a_ "nav-link active" { href: Router.print Router.Home } "Home"
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

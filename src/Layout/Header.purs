module Layout.Header
  ( view
  ) where

import Prelude

import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Router as Router

view :: Router.Route -> ReactElement
view currentRoute =
  H.nav "navbar navbar-light" $
    H.div "container"
    [ H.a_ "navbar-brand" { href: Router.print Router.Home } "conduit"
    , H.ul "nav navbar-nav pull-xs-right" $
      [ navItem { route: Router.Home, label: "Home" }
      , navItem { route: Router.Login, label: "Sign in" }
      , navItem { route: Router.Register, label: "Sign up" }
      ]
    ]
  where
    navItem { route, label } =
      H.li "nav-item" $
        H.a_ ("nav-link" <> if currentRoute == route then " active" else "")
          { href: Router.print route }
          label

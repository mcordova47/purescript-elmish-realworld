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
      [ navItem Router.Home
      , navItem Router.Login
      , navItem Router.Register
      ]
    ]
  where
    navItem route =
      H.li "nav-item" $
        H.a_ ("nav-link" <> if currentRoute == route then " active" else "")
          { href: Router.print route } $
          case route of
            Router.Home -> "Home"
            Router.Article _ -> "Article"
            Router.Login -> "Sign in"
            Router.Register -> "Sign up"

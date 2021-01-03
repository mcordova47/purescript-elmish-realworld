module Layout.Footer
  ( view
  ) where

import Prelude

import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Router as Router
import Utils.Html as Html

view :: ReactElement
view =
  H.footer "" $
    H.div "container"
      [ H.a_ "logo-font" { href: Router.print Router.Home } "conduit"
      , H.span "attribution"
          [ H.text "An interactive learning project from "
          , H.a_ "" { href: "https://thinkster.io" } "Thinkster"
          , H.text ". Code "
          , Html.amp
          , H.text " design licensed under MIT."
          ]
      ]

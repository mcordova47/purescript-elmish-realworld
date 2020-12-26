module Footer
  ( view
  ) where

import Prelude

import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Utils.Html as Html

view :: ReactElement
view =
  H.footer "" $
    H.div "container"
      [ H.a_ "logo-font" { href: "/" } "conduit"
      , H.span "attribution"
          [ H.text "An interactive learning project from "
          , H.a_ "" { href: "https://thinkster.io" } "Thinkster"
          , H.text ". Code "
          , Html.amp
          , H.text " design licensed under MIT."
          ]
      ]

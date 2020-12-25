module Utils.Html
  ( amp
  , nbsp
  ) where

import Elmish (ReactElement)
import Elmish.React.DOM as H

amp :: ReactElement
amp = H.text "\x26"

nbsp :: ReactElement
nbsp = H.text "\xA0"

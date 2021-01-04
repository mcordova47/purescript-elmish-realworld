module Utils.Html
  ( amp
  , attribute
  , eventTarget
  , eventTargetValue
  , nbsp
  ) where

import Prelude

import Data.Maybe (Maybe)
import Elmish (ReactElement)
import Elmish.Foreign (class CanReceiveFromJavaScript, readForeign)
import Elmish.React.DOM as H
import Foreign (Foreign)
import Foreign.Object as F

amp :: ReactElement
amp = H.text "\x26"

nbsp :: ReactElement
nbsp = H.text "\xA0"

eventTarget :: forall a. CanReceiveFromJavaScript a => Foreign -> Maybe a
eventTarget = attribute "target"

attribute :: forall a. CanReceiveFromJavaScript a => String -> Foreign -> Maybe a
attribute name = readForeign >=> F.lookup name >=> readForeign

eventTargetValue :: Foreign -> Maybe String
eventTargetValue = eventTarget >=> attribute "value"

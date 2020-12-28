module Utils.EventHandler
  ( withEvent
  , withPreventDefault
  ) where

import Prelude

import Effect (Effect)
import Elmish (DispatchMsgFn, JsCallback, jsCallback)
import Elmish.Dispatch (issueMsg)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

-- | Some event handlers in Elmish don’t take event arguments (like onClick).
-- `withEvent` allows an event handler with an event argument to act like an
-- event without an event argument to satisfy the type checker.
withEvent :: JsCallback (Foreign -> Effect Unit) -> JsCallback (Effect Unit)
withEvent = unsafeCoerce

-- | Calls `preventDefault` an event before dispatching a message.
withPreventDefault :: DispatchMsgFn Foreign -> JsCallback (Foreign -> Effect Unit)
withPreventDefault dispatch =
  jsCallback \e -> do
    _ :: Unit <- (unsafeCoerce e).preventDefault
    issueMsg dispatch e

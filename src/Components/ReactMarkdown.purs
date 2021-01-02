module Components.ReactMarkdown
  ( reactMarkdown
  ) where

import Elmish (ReactElement, createElement)
import Elmish.React.Import (ImportedReactComponent, ImportedReactComponentConstructor', EmptyProps)

reactMarkdown :: ImportedReactComponentConstructor' EmptyProps EmptyProps (String -> ReactElement)
reactMarkdown =
  createElement reactMarkdown_

foreign import reactMarkdown_ :: ImportedReactComponent

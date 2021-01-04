module Router
  ( Route(..)
  , parse
  , print
  ) where

import Prelude

import Data.Array (filter)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Intertwine (class Syntax, Ctor(..), (*|>), (<|*), (<|:|>), (<|||>))
import Data.Intertwine.Route (PathInfo(..), end, parseRoute, printRoute, seg, segValue)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Foreign.Object as Object

data Route
  = Home
  | Article String
  | Login
  | Register
derive instance eqRoute :: Eq Route
derive instance gRoute :: Generic Route _

routesDef :: forall syntax. Syntax syntax => syntax PathInfo Route
routesDef =
  (Ctor :: Ctor "Home") <|:|> end
  <|||> (Ctor :: Ctor "Article") <|:|> seg "article" *|> segValue <|* end
  <|||> (Ctor :: Ctor "Login") <|:|> seg "login" <|* end
  <|||> (Ctor :: Ctor "Register") <|:|> seg "register" <|* end

print :: Route -> String
print =
  ("#/" <> _) <<< fromMaybe "" <<< map printPathInfo <<< printRoute routesDef
  where
    printPathInfo (PathInfo segments _) = intercalate "/" segments

parse :: String -> Maybe Route
parse hash = hash
  # String.stripPrefix (Pattern "#")
  # fromMaybe hash
  # String.split (Pattern "/")
  # filter (not <<< String.null)
  # pathInfo
  # parseRoute routesDef
  where
    pathInfo segments =
      PathInfo segments Object.empty

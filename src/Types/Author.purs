module Types.Author
  ( Author(..)
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Maybe (Maybe)

newtype Author = Author
  { bio :: Maybe String
  , following :: Boolean
  , image :: String
  , username :: String
  }

instance decodeJsonAuthor :: DecodeJson Author where
  decodeJson json = do
    obj <- decodeJson json
    bio <- obj .:? "bio"
    following <- obj .: "following"
    image <- obj .: "image"
    username <- obj .: "username"
    pure $ Author
      { bio
      , following
      , image
      , username
      }

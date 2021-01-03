module Types.Profile
  ( Profile(..)
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Maybe (Maybe)

newtype Profile = Profile
  { bio :: Maybe String
  , following :: Boolean
  , image :: String
  , username :: String
  }

instance decodeJsonProfile :: DecodeJson Profile where
  decodeJson json = do
    obj <- decodeJson json
    bio <- obj .:? "bio"
    following <- obj .: "following"
    image <- obj .: "image"
    username <- obj .: "username"
    pure $ Profile
      { bio
      , following
      , image
      , username
      }

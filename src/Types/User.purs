module Types.User
  ( User(..)
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Maybe (Maybe)

newtype User = User
  { bio :: Maybe String
  , email :: String
  , image :: Maybe String
  , token :: String
  , username :: String
  }

instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    obj <- decodeJson json
    bio <- obj .:? "bio"
    email <- obj .: "email"
    image <- obj .:? "image"
    token <- obj .: "token"
    username <- obj .: "username"
    pure $ User
      { bio
      , email
      , image
      , token
      , username
      }

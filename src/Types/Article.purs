module Types.Article
  ( Article(..)
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.DateTime (DateTime)
import Types.Author (Author)
import Types.DateTime.Iso (Iso(..))

newtype Article = Article
  { author :: Author
  , body :: String
  , createdAt :: DateTime
  , description :: String
  , favorited :: Boolean
  , favoritesCount :: Int
  , slug :: String
  , tagList :: Array String
  , title :: String
  , updatedAt :: DateTime
  }

instance decodeJsonArticle :: DecodeJson Article where
  decodeJson json = do
    obj <- decodeJson json
    author <- obj .: "author"
    body <- obj .: "body"
    description <- obj .: "description"
    Iso createdAt <- obj .: "createdAt"
    favorited <- obj .: "favorited"
    favoritesCount <- obj .: "favoritesCount"
    slug <- obj .: "slug"
    tagList <- obj .: "tagList"
    title <- obj .: "title"
    Iso updatedAt <- obj .: "updatedAt"
    pure $ Article
      { author
      , body
      , createdAt
      , description
      , favorited
      , favoritesCount
      , slug
      , tagList
      , title
      , updatedAt
      }

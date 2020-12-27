module Api
  ( ArticlesResponse(..)
  , Error(..)
  , TagsResponse(..)
  , articles
  , tags
  ) where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat (json)
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (notElem)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (error)
import Types.Article (Article)

data Error
  = HttpError Affjax.Error
  | ParseError JsonDecodeError

baseUrl :: String
baseUrl = "https://conduit.productionready.io/api"

newtype ArticlesResponse = ArticlesResponse
  { articles :: Array Article
  , articlesCount :: Int
  }
derive instance gArticlesResponse :: Generic ArticlesResponse _
instance decodeJsonArticlesResponse :: DecodeJson ArticlesResponse where
  decodeJson json = do
    obj <- decodeJson json
    articles' <- obj .: "articles"
    articlesCount <- obj .: "articlesCount"
    pure $ ArticlesResponse { articles: articles', articlesCount }

articles :: forall m. MonadAff m => Maybe String -> m (Either Error ArticlesResponse)
articles tag = liftAff do
  res <- Affjax.get json $ baseUrl <> "/articles" <> maybe "" ("?tag=" <> _) tag
  pure $ res
    # lmap HttpError
    >>= \r ->
      -- Turns out, Affjax doesn't report some error codes as errors. One example
      -- I discovered is 403. So we turn non-200 codes into errors here. 200
      -- should be enough for us for now, and we can always generalize this logic
      -- later.
      if r.status `notElem` [StatusCode 200, StatusCode 204]
        then Left $ HttpError $ Affjax.XHRError $ error $ show r.status
        else lmap ParseError $ responseBody r
  where
    responseBody :: Affjax.Response Json -> Either JsonDecodeError ArticlesResponse
    responseBody r =  decodeJson r.body

newtype TagsResponse = TagsResponse
  { tags :: Array String
  }
derive instance gTagsResponse :: Generic TagsResponse _
instance decodeJsonTagsResponse :: DecodeJson TagsResponse where
  decodeJson json = do
    obj <- decodeJson json
    tags' <- obj .: "tags"
    pure $ TagsResponse { tags: tags' }

tags :: forall m. MonadAff m => m (Either Error TagsResponse)
tags = liftAff do
  res <- Affjax.get json $ baseUrl <> "/tags"
  pure $ res
    # lmap HttpError
    >>= \r ->
      -- Turns out, Affjax doesn't report some error codes as errors. One example
      -- I discovered is 403. So we turn non-200 codes into errors here. 200
      -- should be enough for us for now, and we can always generalize this logic
      -- later.
      if r.status `notElem` [StatusCode 200, StatusCode 204]
        then Left $ HttpError $ Affjax.XHRError $ error $ show r.status
        else lmap ParseError $ responseBody r
  where
    responseBody :: Affjax.Response Json -> Either JsonDecodeError TagsResponse
    responseBody r =  decodeJson r.body

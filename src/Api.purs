module Api
  ( ArticleResponse(..)
  , ArticlesResponse(..)
  , Error(..)
  , TagsResponse(..)
  , article
  , articles
  , tags
  ) where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat (json)
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate, notElem)
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (error)
import Types.Article (Article)

data Error
  = HttpError Affjax.Error
  | ParseError JsonDecodeError

baseUrl :: String
baseUrl = "https://conduit.productionready.io/api"

type ArticlesRequest =
  { limit :: Maybe Int
  , offset :: Maybe Int
  , tag :: Maybe String
  }

newtype ArticlesResponse = ArticlesResponse
  { articles :: Array Article
  , articlesCount :: Int
  }
instance decodeJsonArticlesResponse :: DecodeJson ArticlesResponse where
  decodeJson json = do
    obj <- decodeJson json
    articles' <- obj .: "articles"
    articlesCount <- obj .: "articlesCount"
    pure $ ArticlesResponse { articles: articles', articlesCount }

articles :: forall m. MonadAff m => ArticlesRequest -> m (Either Error ArticlesResponse)
articles { limit, offset, tag} = liftAff do
  res <- Affjax.get json $ baseUrl <> "/articles" <> query
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
    responseBody r = decodeJson r.body

    query = queryString $ catMaybes
      [ { key: "limit", value: _ } <<< show <$> limit
      , { key: "offset", value: _ } <<< show <$> offset
      , { key: "tag", value: _ } <$> tag
      ]

newtype ArticleResponse = ArticleResponse
  { article :: Article
  }
instance decodeJsonArticleResponse :: DecodeJson ArticleResponse where
  decodeJson json = do
    obj <- decodeJson json
    article' <- obj .: "article"
    pure $ ArticleResponse { article: article' }

article :: forall m. MonadAff m => String -> m (Either Error ArticleResponse)
article slug = liftAff do
  res <- Affjax.get json $ baseUrl <> "/articles/" <> slug
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
    responseBody r = decodeJson r.body

newtype TagsResponse = TagsResponse
  { tags :: Array String
  }
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

queryString :: Array { key :: String, value :: String } -> String
queryString [] = ""
queryString params = params
  <#> (\{ key, value } -> key <> "=" <> value)
  # intercalate "&"
  # ("?" <> _)

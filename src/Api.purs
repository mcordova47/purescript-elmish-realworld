module Api
  ( ArticleResponse(..)
  , ArticlesResponse(..)
  , Error(..)
  , LoginRequest(..)
  , RegisterRequest(..)
  , TagsResponse(..)
  , UserResponse(..)
  , article
  , articles
  , login
  , register
  , tags
  ) where

import Prelude

import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat (json)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Types.Article (Article)
import Types.User (User)

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
    >>= \r -> lmap ParseError $ responseBody r
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
    >>= \r -> lmap ParseError $ responseBody r
  where
    responseBody r = decodeJson r.body

newtype UserResponse = UserResponse
  { user :: User
  }
instance decodeJsonUserResponse :: DecodeJson UserResponse where
  decodeJson json = do
    obj <- decodeJson json
    user <- obj .: "user"
    pure $ UserResponse { user }

newtype LoginRequest = LoginRequest
  { user ::
      { email :: String
      , password :: String
      }
  }
derive instance gLoginRequest :: Generic LoginRequest _
instance encodeJsonLoginRequest :: EncodeJson LoginRequest where
  encodeJson = genericEncodeJson

login :: forall m. MonadAff m => LoginRequest -> m (Either Error UserResponse)
login req = liftAff do
  res <- Affjax.post json (baseUrl <> "/login") requestBody
  pure $ res
    # lmap HttpError
    >>= \r -> lmap ParseError $ responseBody r
  where
    requestBody =
      Just $ RequestBody.json $ encodeJson req
    responseBody r =
      decodeJson r.body

newtype RegisterRequest = RegisterRequest
  { user ::
      { email :: String
      , password :: String
      , username :: String
      }
  }
derive instance gRegisterRequest :: Generic RegisterRequest _
instance encodeJsonRegisterRequest :: EncodeJson RegisterRequest where
  encodeJson = genericEncodeJson

register :: forall m. MonadAff m => RegisterRequest -> m (Either Error UserResponse)
register req = liftAff do
  res <- Affjax.post json (baseUrl <> "/users") requestBody
  pure $ res
    # lmap HttpError
    >>= \r -> lmap ParseError $ responseBody r
  where
    requestBody =
      Just $ RequestBody.json $ encodeJson req
    responseBody r =
      decodeJson r.body

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
    >>= \r -> lmap ParseError $ responseBody r
  where
    responseBody :: Affjax.Response Json -> Either JsonDecodeError TagsResponse
    responseBody r =  decodeJson r.body

queryString :: Array { key :: String, value :: String } -> String
queryString [] = ""
queryString params = params
  <#> (\{ key, value } -> key <> "=" <> value)
  # intercalate "&"
  # ("?" <> _)

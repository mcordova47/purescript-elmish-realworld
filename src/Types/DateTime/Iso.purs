module Types.DateTime.Iso
  ( Iso(..)
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.JSDate (parse, toDateTime)
import Data.Maybe (maybe)
import Effect.Unsafe (unsafePerformEffect)

newtype Iso = Iso DateTime

instance decodeJsonIso :: DecodeJson Iso where
  decodeJson json = do
    dateString <- decodeJson json
    let jsDate = unsafePerformEffect $ parse dateString
    maybe (Left $ UnexpectedValue json) (pure <<< Iso) $ toDateTime jsDate

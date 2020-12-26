module Utils.Number
  ( formatAsOrdinal
  ) where

import Prelude

import Data.Foldable (elem)
import Data.Ord (abs)

formatAsOrdinal :: Int -> String
formatAsOrdinal n =
  show n <> suffix
  where
    suffix =
      case abs n of
        n' | n' `elem` [11, 12, 13] -> "th"
        n' -> case n' `mod` 10 of
          1 -> "st"
          2 -> "nd"
          _ -> "th"

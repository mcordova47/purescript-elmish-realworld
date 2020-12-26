module Utils.Number
  ( formatAsOrdinal
  ) where

import Prelude

import Data.Ord (abs)

formatAsOrdinal :: Int -> String
formatAsOrdinal n =
  show n <> suffix
  where
    suffix = case absoluteValue `mod` 10 of
      1 | absoluteValue `mod` 100 /= 11 -> "st"
      2 | absoluteValue `mod` 100 /= 12 -> "nd"
      3 | absoluteValue `mod` 100 /= 13 -> "rd"
      _ -> "th"
    absoluteValue =
      abs n

module Utils.Number
  ( formatAsOrdinal
  ) where

import Prelude

import Data.Ord (abs)

formatAsOrdinal :: Int -> String
formatAsOrdinal n =
  show n <> suffix
  where
    suffix = case remander100 of
      11 -> "th"
      12 -> "th"
      13 -> "th"
      _ -> case remander10 of
        1 -> "st"
        2 -> "nd"
        3 -> "rd"
        _ -> "th"
    absN =
      abs n
    remander100 = 
      absN `mod` 100
    remander10 = 
      absN `mod` 10

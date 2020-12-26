module Utils.DateTime
  ( formatAsDate
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime as Date
import Data.DateTime as DateTime
import Data.Enum (fromEnum)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List(..), (:))
import Utils.Number as Number

formatAsDate :: DateTime -> String
formatAsDate dt =
  format (MonthFull : Nil) dt <>
  Number.formatAsOrdinal (fromEnum $ Date.day $ DateTime.date dt)

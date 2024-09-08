{-# LANGUAGE OverloadedRecordDot #-}

module Format
  ( tableFormat
  ) where

import Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import MortgageCalculator (Event(..), cents)
import Text.Layout.Table

tableFormat :: [Event] -> String
tableFormat events = tableString $ columnHeaderTableS columnSpecs unicodeS tableHeaders tableRows
  where
    columnSpecs = replicate (length headers) numCol
    tableHeaders = titlesH headers
    tableRows = rowG . fieldList <$> events

headers :: [String]
headers =
  [ "date"
  , "correct amount due"
  , "actual interest paid"
  , "principal paid"
  , "correct current principal (after payment)"
  ]

fieldList :: Event -> [String]
fieldList e =
  [ formatShow iso8601Format e.date
  , show $ cents e.amountDue
  , show $ cents e.interestPaid
  , show $ cents e.principalPaid
  , show $ cents e.currentPrincipal
  ]

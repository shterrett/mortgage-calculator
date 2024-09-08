{-# LANGUAGE OverloadedRecordDot #-}

module Format
  ( tableFormat
  ) where

import Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import MortgageCalculator (Event(..))
import Text.Layout.Table

tableFormat :: [Event] -> String
tableFormat events = unlines [tableHeaders, tableRows]
  where
    tableHeaders :: String
    tableHeaders = gridString [column expand center def def] [headers]
    tableRows :: String
    tableRows = gridString (replicate (length events) numCol) (fieldList <$> events)

headers :: [String]
headers =
  [ "date"
  , "amountDue"
  , "interestPaid"
  , "principalPaid"
  , "currentMonthInterest"
  , "currentPrincipal"
  ]

fieldList :: Event -> [String]
fieldList e =
  [ formatShow iso8601Format e.date
  , show e.amountDue
  , show e.interestPaid
  , show e.principalPaid
  , show e.currentMonthInterest
  , show e.currentPrincipal
  ]

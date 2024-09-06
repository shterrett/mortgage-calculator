{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Parser
import Data.Maybe (fromMaybe)
import Data.Time (Day)
import Data.Time.Format.ISO8601 (formatParseM, iso8601Format)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Payment parser" $ do
  it "parses a row with principal and interest payments" $ do
    parsePayments "2024-07-01 p:100 i:1\n"
      `shouldBe` (Right [Payment (day "2024-07-01") 100.00 1.00])
  it "parses a row with principal and interest payments in the other order" $ do
    parsePayments "2024-07-01 i:1 p:100\n"
      `shouldBe` (Right [Payment (day "2024-07-01") 100.00 1.00])
  it "parses a row with only a principal payment" $ do
    parsePayments "2024-07-01 p:100\n"
      `shouldBe` (Right [Payment (day "2024-07-01") 100.00 0.00])
  it "parses a row with only an interest payment" $ do
    parsePayments "2024-07-01 i:1\n"
      `shouldBe` (Right [Payment (day "2024-07-01") 0.00 1.00])
  it "parses multiple records" $ do
    parsePayments "2024-07-01 i:1\n2024-08-01 p:100\n"
      `shouldBe` (Right [Payment (day "2024-07-01") 0.00 1.00
                        , Payment (day "2024-08-01") 100.00 0.00
                        ])
  it "parses an empty list" $
    parsePayments "" `shouldBe` (Right [])

day :: String -> Day
day = fromMaybe (error "Invalid Date") . formatParseM iso8601Format

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Text.IO qualified as Text
import Format (tableFormat)
import MortgageCalculator (calculate, Mortgage(..), Payment(..), dollars, apr)
import Options (Options(..), unwrapRecord)
import Parser (parsePayments)

main :: IO ()
main = do
  Options{..} <- unwrapRecord "mortgage-calculator"
  paymentFile <- Text.readFile payments
  case parsePayments paymentFile of
    Left e -> error e
    Right ps -> do
      let
        end = maybe (maximum $ (\Payment{..} -> date) <$> ps) id endDate
        mortgage = Mortgage { initialPrincipal = dollars initialPrincipal
                            , interestRate = apr interestRate
                            , extraInterest = extraInterest
                            }
        events = calculate startDate end mortgage ps showAll
      putStrLn $ tableFormat events

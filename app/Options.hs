{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Options
  ( Options(..)
  , unwrapRecord
  ) where

import Options.Generic
import Data.Time (Day)
import MortgageCalculator(ExtraInterest)

data Options w = Options
  { showAll :: w ::: Bool <?> "Show balances every day in the range" <!> "False"
  , startDate :: w ::: Day <?> "Start date for calculations"
  , endDate :: w ::: Maybe Day <?> "End date for calculations. Defaults to last entry in payments"
  , payments :: w ::: FilePath <?> "Location of payment record. Line-oriented, formatted as 'YYYY-MM-DD p:123.45 i:123.45' where either p or i may be omitted, and represent principal and interest payments respectively"
  , initialPrincipal :: w ::: Double <?> "Initial principal value"
  , extraInterest :: w ::: ExtraInterest <?> "How to handle excess interest payment"
  , interestRate :: w ::: Double <?> "Interest Rate (annual)"
  }
  deriving (Generic)

instance ParseRecord (Options Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving instance Show (Options Unwrapped)

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module MortgageCalculator
  ( ExtraInterest(..)
  , Dollars
  , dollars
  , Apr
  , apr
  , Payment(..)
  , Event(..)
  , calculate
  ) where

import Control.Monad.State (StateT, MonadState, evalStateT)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Writer.CPS (WriterT, MonadWriter, execWriterT)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Decimal (Decimal, realFracToDecimal)
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.Calendar (addDays)
import Options.Generic (ParseField, ParseFields, ParseRecord, Generic)

newtype Dollars = Dollars Decimal
  deriving newtype (Show)

dollars :: Double -> Dollars
dollars = Dollars . realFracToDecimal 2

newtype Apr = Apr Decimal
  deriving newtype (Show)

apr :: Double -> Apr
apr = Apr . realFracToDecimal 4

-- | Basic data about the Mortgage; does not change
data Mortgage = Mortgage
  { initialPrincipal :: Dollars
  , interestRate :: Apr
  , extraInterest :: ExtraInterest
  }
  deriving (Show)

-- | How to apply the amount of an interest payment that is in excess of the due
-- payment
data ExtraInterest =
  TowardsPrincipal -- ^ The excess is immediately rolled over and applied against the principal
  | NegativeBalance -- ^ The excess is applied against the next interest due calculation
  deriving (Show, Generic, Eq, Read)
  deriving anyclass (ParseField, ParseFields, ParseRecord)

-- | Recorded payment made on a certain day
data Payment = Payment
  { date :: Day
  , principalAmount :: Decimal
  , interestAmount :: Decimal
  }
  deriving (Show, Eq)

-- | An interesting event: this is either a payment or it's the first of a month
-- when an amount due is posted. These may happen on the same day, and will be
-- packaged into the same Event. One Event per day
data Event = Event
  { date :: Day
  , amountDue :: Dollars
  , interestPaid :: Dollars
  , principalPaid :: Dollars
  , accruedInterest :: Dollars
  , currentPrincipal :: Dollars
  }
  deriving (Show)

data Account

newtype Calculator a = Calculator
  { unCalculator :: WriterT [Event] (StateT Account (ExceptT Text Identity)) a
  }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadWriter [Event]
                   , MonadState Account
                   , MonadError Text
                   )

runCalculator :: Account -> Calculator a -> Either Text [Event]
runCalculator initial (Calculator m) =
  runIdentity
  $ runExceptT
  $ flip evalStateT initial
  $ execWriterT m

calculate ::
  Day -- ^ start date
  -> Day -- ^ end date
  -> Mortgage
  -> [Payment] -- ^ Record of payments made
  -> Bool -- ^ create an Event for every day regardless of whether anything interesting happened
  -> [Event]
calculate start end mortgage payments showAll = undefined
  where
    dateRange :: [Day]
    dateRange = [start .. end]
    nextDay :: Day -> Day
    nextDay = addDays 1

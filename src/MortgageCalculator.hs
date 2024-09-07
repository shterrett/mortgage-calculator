{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImportQualifiedPost #-}
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

import Control.Monad (forM_, when)
import Control.Monad.State (StateT, MonadState, evalStateT, modify, get)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Writer.CPS (WriterT, MonadWriter, execWriterT, tell)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Decimal (Decimal, realFracToDecimal, divide)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (Day, toGregorian)
import Options.Generic (ParseField, ParseFields, ParseRecord, Generic)

newtype Dollars = Dollars Decimal
  deriving newtype (Show, Eq, Ord, Num)

dollars :: Double -> Dollars
dollars = Dollars . realFracToDecimal 2

newtype Apr = Apr Decimal
  deriving newtype (Show, Eq, Ord, Num)

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
  , principalAmount :: Dollars
  , interestAmount :: Dollars
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
  , currentMonthInterest :: Dollars
  , currentPrincipal :: Dollars
  }
  deriving (Show)

data Account = Account
  { accruedInterest :: Dollars -- ^ The interest currently accrued for the current month
  , balanceDue :: Dollars -- ^ The interest due from the previous month.
                          -- This will be 0 if it has been paid, and may be
                          -- negative if an interest payment greater than the
                          -- due payment is applied to future interest payments.
  , principal :: Dollars -- ^ the remaining principal, reflecting all principal
                         -- payments applied to this point
  }
  deriving (Show)

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
runCalculator initial =
  runIdentity
  . runExceptT
  . flip evalStateT initial
  . execWriterT
  . unCalculator

calculate ::
  Day -- ^ start date
  -> Day -- ^ end date
  -> Mortgage
  -> [Payment] -- ^ Record of payments made
  -> Bool -- ^ create an Event for every day regardless of whether anything interesting happened
  -> Either Text [Event]
calculate start end mortgage payments showAll =
  runCalculator initialAccount $ calculate' showAll mortgage dateRange paymentMap
  where
    dateRange :: [Day]
    dateRange = [start .. end]
    initialAccount = Account { accruedInterest = dollars 0
                             , balanceDue = dollars 0
                             , principal = mortgage.initialPrincipal
                             }
    paymentMap = Map.fromList $ (\p -> (p.date, p)) <$> payments

calculate' ::
  Bool
  -> Mortgage
  -> [Day]
  -> Map Day Payment
  -> Calculator ()
calculate' showAll mortgage range payments = do
  forM_ range $ \day -> do
    when (firstOfMonth day) setInterestDue
    accrueInterest
    let payment = Map.lookup day payments
    maybe (pure ()) applyPayment payment
    when (interesting day payment) $ emitEvent day payment

  where
    firstOfMonth :: Day -> Bool
    firstOfMonth d =
      case toGregorian d of
        (_, _, dayOfMonth) -> dayOfMonth == 1
    setInterestDue :: Calculator ()
    setInterestDue = modify $ \a ->
      a { balanceDue = a.balanceDue + a.accruedInterest
        , accruedInterest = dollars 0
        }
    accrueInterest :: Calculator ()
    accrueInterest = modify $ \a ->
      a { accruedInterest = a.accruedInterest + calculateInterest a.principal mortgage.interestRate }
    applyPayment :: Payment -> Calculator ()
    applyPayment p = modify $ \a ->
      let
        (appliedInterest, excessInterest) =
          if p.interestAmount <= a.balanceDue
            then (p.interestAmount, dollars 0)
            else (a.balanceDue, p.interestAmount - a.balanceDue)
      in
        case mortgage.extraInterest of
          TowardsPrincipal ->
            a { principal = a.principal - p.principalAmount - excessInterest
              , balanceDue = a.balanceDue - appliedInterest
              }
          NegativeBalance ->
            a { principal = a.principal - p.principalAmount
              , balanceDue = a.balanceDue - appliedInterest - excessInterest
              }
    interesting :: Day -> Maybe Payment -> Bool
    interesting day mPayment = showAll || firstOfMonth day || isJust mPayment
    emitEvent :: Day -> Maybe Payment -> Calculator ()
    emitEvent day mPayment = do
      a <- get
      let
        event = Event
          { date = day
          , amountDue = a.balanceDue
          , interestPaid = maybe (dollars 0) interestAmount mPayment
          , principalPaid = maybe (dollars 0) principalAmount mPayment
          , currentMonthInterest = a.accruedInterest
          , currentPrincipal = a.principal
          }
      tell [event]
    calculateInterest :: Dollars -> Apr -> Dollars
    calculateInterest (Dollars p) (Apr r) =
      case (p * r) `divide` 365 of
        (_, amt) : _ -> Dollars amt
        _ -> error "calculation of interest yielded no results"


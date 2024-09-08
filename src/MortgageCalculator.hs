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
  , cents
  , Apr
  , apr
  , Payment(..)
  , Event(..)
  , calculate
  , Mortgage(..)
  ) where

import Control.Monad (forM_, when)
import Control.Monad.State (State, MonadState, evalState, modify, get, gets)
import Control.Monad.Writer.CPS (WriterT, MonadWriter, execWriterT, tell)
import Data.Decimal (Decimal, realFracToDecimal, divide, roundTo)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Time (Day, toGregorian)
import Options.Generic (ParseField, ParseFields, ParseRecord, Generic)

newtype Dollars = Dollars Decimal
  deriving newtype (Show, Eq, Ord, Num)

dollars :: Double -> Dollars
dollars = Dollars . realFracToDecimal 2

cents :: Dollars -> Dollars
cents (Dollars d) = Dollars $ roundTo 2 d

newtype Apr = Apr Decimal
  deriving newtype (Show, Eq, Ord, Num)

apr :: Double -> Apr
apr = Apr . realFracToDecimal 5

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
  { unCalculator :: WriterT [Event] (State Account) a
  }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadWriter [Event]
                   , MonadState Account
                   )

runCalculator :: Account -> Calculator a -> [Event]
runCalculator initial =
  flip evalState initial
  . execWriterT
  . unCalculator

-- | The core logic: Calculates the daily interest, balance, and principal for
-- the mortgage, account for payments made on the day they are submitted.
-- On each day:
--   - if it is the first of the month, the accrued interest is added to the
--   balance due and set to 0
--   - interest for the day is accrued (current principal * apr / 365)
--   - payments are applied. Depending on the option, excess interest will
--   either immediately reduce the principal, or allow the balance due to go
--   negative. "Excess" is calculated against only the balance due and not any
--   interst accrued during the month (ie, if the payment is made on the 5th,
--   the interest accrued from the 1st - the 5th is not added to the balance
--   due)
--   - If the day is "interesting" (it's the 1st, a payment was made, or the
--   showAll flag was passed) an event with the current state of the account
--   is emitted. This will be the final output
calculate ::
  Day -- ^ start date
  -> Day -- ^ end date
  -> Mortgage
  -> [Payment] -- ^ Record of payments made
  -> Bool -- ^ create an Event for every day regardless of whether anything interesting happened
  -> [Event]
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
    currentBalance <- gets balanceDue
    let payment = Map.lookup day payments
    maybe (pure ()) applyPayment payment
    when (interesting day payment) $ emitEvent day currentBalance payment

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
    emitEvent :: Day -> Dollars -> Maybe Payment -> Calculator ()
    emitEvent day currentBalance mPayment = do
      a <- get
      -- currentBalance reflects the balance _before_ the payment is applied
      let
        event = Event
          { date = day
          , amountDue = currentBalance
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


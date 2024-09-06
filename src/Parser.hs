{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Parser
  ( parsePayments
  , Payment(..)
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Decimal (Decimal, realFracToDecimal)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import Data.Time.Format.ISO8601 (formatParseM, iso8601Format)

data Payment = Payment
  { date :: Day
  , principalAmount :: Decimal
  , interestAmount :: Decimal
  }
  deriving (Show, Eq)

parsePayments :: Text -> Either String [Payment]
parsePayments = parseOnly (pp <* endOfInput)
  where
    pp :: Parser [Payment]
    pp = many' (parsePayment <* endOfLine)

parsePayment :: Parser Payment
parsePayment = do
  theDate <- parseDate
  _ <- skipSpace
  (principal, interest) <-
    forwards
    <|> backwards
    <|> onlyInterest
    <|> onlyPrincipal
  pure $ Payment
    { date = theDate
    , principalAmount = fromMaybe 0 principal
    , interestAmount = fromMaybe 0 interest
    }
  where
    forwards :: Parser (Maybe Decimal, Maybe Decimal)
    forwards = do
      p <- parsePrincipal
      _ <- skipSpace
      i <- parseInterest
      pure (Just p, Just i)
    backwards :: Parser (Maybe Decimal, Maybe Decimal)
    backwards = do
      i <- parseInterest
      _ <- skipSpace
      p <- parsePrincipal
      pure (Just p, Just i)
    onlyPrincipal :: Parser (Maybe Decimal, Maybe Decimal)
    onlyPrincipal = (,Nothing) . Just <$> parsePrincipal
    onlyInterest :: Parser (Maybe Decimal, Maybe Decimal)
    onlyInterest = (Nothing,) . Just <$> parseInterest

parseDate :: Parser Day
parseDate = do
  dayString <- fmap Text.unpack $ takeTill isSpace
  formatParseM iso8601Format dayString

parsePrincipal :: Parser Decimal
parsePrincipal = fmap (realFracToDecimal 2) $ char 'p' *> char ':' *> double

parseInterest :: Parser Decimal
parseInterest = fmap (realFracToDecimal 2) $ char 'i' *> char ':' *> double

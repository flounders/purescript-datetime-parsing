module
  Data.DateTime.Parsing ( Direction(..)
                        , FullDateTime
                        , Offset(..)
                        , parseFullDateTime
                        ) where

import Prelude

import Data.Array as Array
import Data.DateTime as DT
import Data.Enum (toEnum)
import Data.Foldable as F
import Data.Int (floor)
import Data.List.Types (toList)
import Data.Maybe (Maybe, maybe)
import Data.Number (fromString)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT

data Offset = Zulu
            | Offset Direction DT.Hour DT.Minute

instance showOffset :: Show Offset where
  show Zulu = "(Offset Zulu)"
  show (Offset d h m) = "(Offset " <> show d <> " " <> show h <> ":" <> show m <> ")"

data Direction = Plus
               | Minus

instance showDirection :: Show Direction where
  show Plus = "+"
  show Minus = "-"

data FullDateTime = FullDateTime DT.DateTime Offset

instance showFullDateTime :: Show FullDateTime where
  show (FullDateTime dt offset) = "(FullDateTime " <> show dt <> " " <> show offset <> ")"

-- Parser helpers
count :: forall s m a. Monad m => Int -> P.ParserT s m a -> P.ParserT s m (Array a)
count n p = sequence $ Array.replicate n p

parseDigits :: forall m. Monad m => Int -> P.ParserT String m Int
parseDigits n = do
  digits <- count n PT.digit
  maybe (P.fail "bad number") (pure <<< floor) <<< fromString <<< fromCharArray $ digits

maybeFail :: forall s m a. Monad m => String -> Maybe a -> P.ParserT s m a
maybeFail str = maybe (P.fail str) pure


-- Date parsers
parseYear :: forall m. Monad m => P.ParserT String m DT.Year
parseYear = do
  digits <- parseDigits 4
  maybeFail "bad year" $ toEnum digits

parseMonth :: forall m. Monad m => P.ParserT String m DT.Month
parseMonth = do
  digits <- parseDigits 2
  maybeFail "bad month" $ toEnum digits

parseDay :: forall m. Monad m => P.ParserT String m DT.Day
parseDay = do
  digits <- parseDigits 2
  maybeFail "bad day" $ toEnum digits

parseDate :: forall m. Monad m => P.ParserT String m DT.Date
parseDate = do
  year <- parseYear
  _ <- PS.char '-'
  month <- parseMonth
  _ <- PS.char '-'
  day <- parseDay
  maybeFail "bad date" $ DT.exactDate year month day


-- Time parsers
parseHour :: forall m. Monad m => P.ParserT String m DT.Hour
parseHour = do
  digits <- parseDigits 2
  maybeFail "bad hour" $ toEnum digits

parseMinute :: forall m. Monad m => P.ParserT String m DT.Minute
parseMinute = do
  digits <- parseDigits 2
  maybeFail "bad minute" $ toEnum digits

parseSecond :: forall m. Monad m => P.ParserT String m DT.Second
parseSecond = do
  digits <- parseDigits 2
  maybeFail "bad second" $ toEnum digits

-- RFC3339 does not specify a maximum number of digits for the second fraction
-- portion. Even though Time can only offer precision to a millisecond, there is
-- still a need to handle any extra digits that could possibly show up and ignore
-- them.
parseSecondFraction :: forall m. Monad m => P.ParserT String m DT.Millisecond
parseSecondFraction = do
  _ <- PS.char '.'
  digits <- PC.many1 PT.digit
  let arrayOfDigits = Array.concat <<< (\x -> [['0', '.'], x]) <<< Array.take 3 <<< F.foldMap (Array.singleton) <<< toList $ digits
      maybeN = fromString <<< fromCharArray $ arrayOfDigits
      millisecond :: Maybe DT.Millisecond
      millisecond = do
         n <- maybeN
         toEnum <<< floor $ n * 1000.0
  maybeFail "bad millisecond" $ millisecond

parsePartialTime :: forall m. Monad m => P.ParserT String m DT.Time
parsePartialTime = do
  hh <- parseHour
  _ <- PS.char ':'
  mm <- parseMinute
  _ <- PS.char ':'
  ss <- parseSecond
  default_mss <- maybeFail "bad default millisecond" $ toEnum 0
  mss <- PC.option default_mss parseSecondFraction
  pure $ DT.Time hh mm ss mss

parseOffset :: forall m. Monad m => P.ParserT String m Offset
parseOffset = PC.choice [ parseZuluOffset
                        , parseNumOffset ]

parseZuluOffset :: forall m. Monad m => P.ParserT String m Offset
parseZuluOffset = do
  _ <- PC.choice [ PS.char 'Z'
                 , PS.char 'z' ]
  pure Zulu

parseDirection :: forall m. Monad m => P.ParserT String m Direction
parseDirection = do
  direction <- PC.choice [ PS.char '+'
                         , PS.char '-' ]
  case direction of
      '+' -> pure Plus
      '-' -> pure Minus
      _ -> P.fail "Bad direction"

parseNumOffset :: forall m. Monad m => P.ParserT String m Offset
parseNumOffset = do
  direction <- parseDirection
  hour <- parseHour
  _ <- PS.char ':'
  minute <- parseMinute
  pure $ Offset direction hour minute

parseFullDateTime :: forall m. Monad m => P.ParserT String m FullDateTime
parseFullDateTime = do
  date <- parseDate
  _ <- PC.choice [ PS.char 'T' , PS.char 't' ]
  time <- parsePartialTime
  offset <- parseOffset
  pure $ FullDateTime (DT.DateTime date time) offset

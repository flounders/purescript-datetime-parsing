module
  Data.DateTime.Parsing ( Direction(..)
                        , Offset(..)
                        , FullDateTime(..)
                        , parseFullDateTime
                        , fromString
                        , toUTC
                        ) where

import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode (isDecDigit)
import Data.DateTime as DT
import Data.Time as T
import Data.Time.Duration as D
import Data.Either (Either)
import Data.Enum (toEnum)
import Data.Enum as E
import Data.Foldable as F
import Data.Int (floor)
import Data.List.Types (toList)
import Data.Maybe (Maybe, maybe)
import Data.Number as N
import Data.String.CodeUnits (fromCharArray)
import Data.String.CodePoints (codePointFromChar)
import Data.Traversable (sequence)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

data Offset = Zulu
            | Offset Direction DT.Hour DT.Minute

instance showOffset :: Show Offset where
  show Zulu = "(Offset Zulu)"
  show (Offset d h m) = "(Offset " <> show d <> " " <> show h <> ":" <> show m <> ")"

derive instance Eq Offset

data Direction = Plus
               | Minus

instance showDirection :: Show Direction where
  show Plus = "+"
  show Minus = "-"

derive instance Eq Direction

data FullDateTime = FullDateTime DT.DateTime Offset

instance showFullDateTime :: Show FullDateTime where
  show (FullDateTime dt offset) = "(FullDateTime " <> show dt <> " " <> show offset <> ")"

derive instance Eq FullDateTime

-- Parser helpers
count :: forall s m a. Monad m => Int -> P.ParserT s m a -> P.ParserT s m (Array a)
count n p = sequence $ Array.replicate n p

digit :: forall s m. PS.StringLike s => Monad m => P.ParserT s m Char
digit = PS.satisfy (isDecDigit <<< codePointFromChar)

parseDigits :: forall s m. PS.StringLike s => Monad m => Int -> P.ParserT s m Int
parseDigits n = do
  digits <- count n digit
  maybe (P.fail "bad number") (pure <<< floor) <<< N.fromString <<< fromCharArray $ digits

maybeFail :: forall s m a. Monad m => String -> Maybe a -> P.ParserT s m a
maybeFail str = maybe (P.fail str) pure

parseDateTimeSegment :: forall s m a. PS.StringLike s => Monad m => E.BoundedEnum a => String -> Int -> P.ParserT s m a
parseDateTimeSegment errorMessage n = do
  digits <- parseDigits n
  maybeFail errorMessage $ toEnum digits

-- Date parsers
parseYear :: forall s m. PS.StringLike s => Monad m => P.ParserT s m DT.Year
parseYear = parseDateTimeSegment "bad year" 4

parseMonth :: forall s m. PS.StringLike s => Monad m => P.ParserT s m DT.Month
parseMonth = parseDateTimeSegment "bad month" 2

parseDay :: forall s m. PS.StringLike s => Monad m => P.ParserT s m DT.Day
parseDay = parseDateTimeSegment "bad day" 2

parseDate :: forall s m. PS.StringLike s => Monad m => P.ParserT s m DT.Date
parseDate = do
  year <- parseYear
  _ <- PS.char '-'
  month <- parseMonth
  _ <- PS.char '-'
  day <- parseDay
  maybeFail "bad date" $ DT.exactDate year month day


-- Time parsers
parseHour :: forall s m. PS.StringLike s => Monad m => P.ParserT s m DT.Hour
parseHour = parseDateTimeSegment "bad hour" 2

parseMinute :: forall s m. PS.StringLike s => Monad m => P.ParserT s m DT.Minute
parseMinute = parseDateTimeSegment "bad minute" 2

parseSecond :: forall s m. PS.StringLike s => Monad m => P.ParserT s m DT.Second
parseSecond = parseDateTimeSegment "bad second" 2

-- RFC3339 does not specify a maximum number of digits for the second fraction
-- portion. Even though Time can only offer precision to a millisecond, there is
-- still a need to handle any extra digits that could possibly show up and ignore
-- them.
parseSecondFraction :: forall s m. PS.StringLike s => Monad m => P.ParserT s m DT.Millisecond
parseSecondFraction = do
  _ <- PS.char '.'
  digits <- PC.many1 digit
  let arrayOfDigits = Array.concat <<< (\x -> [['0', '.'], x]) <<< Array.take 3 <<< F.foldMap (Array.singleton) <<< toList $ digits
      maybeN = N.fromString <<< fromCharArray $ arrayOfDigits
      millisecond :: Maybe DT.Millisecond
      millisecond = do
         n <- maybeN
         toEnum <<< floor $ n * 1000.0
  maybeFail "bad millisecond" $ millisecond

parsePartialTime :: forall s m. PS.StringLike s => Monad m => P.ParserT s m DT.Time
parsePartialTime = do
  hh <- parseHour
  _ <- PS.char ':'
  mm <- parseMinute
  _ <- PS.char ':'
  ss <- parseSecond
  default_mss <- maybeFail "bad default millisecond" $ toEnum 0
  mss <- PC.option default_mss parseSecondFraction
  pure $ DT.Time hh mm ss mss

parseOffset :: forall s m. PS.StringLike s => Monad m => P.ParserT s m Offset
parseOffset = PC.choice [ parseZuluOffset
                        , parseNumOffset ]

parseZuluOffset :: forall s m. PS.StringLike s => Monad m => P.ParserT s m Offset
parseZuluOffset = do
  _ <- PC.choice [ PS.char 'Z'
                 , PS.char 'z' ]
  pure Zulu

parseDirection :: forall s m. PS.StringLike s => Monad m => P.ParserT s m Direction
parseDirection = do
  direction <- PC.choice [ PS.char '+'
                         , PS.char '-' ]
  case direction of
      '+' -> pure Plus
      '-' -> pure Minus
      _ -> P.fail "Bad direction"

parseNumOffset :: forall s m. PS.StringLike s => Monad m => P.ParserT s m Offset
parseNumOffset = do
  direction <- parseDirection
  hour <- parseHour
  _ <- PS.char ':'
  minute <- parseMinute
  pure $ Offset direction hour minute

-- | `parseFullDateTime` will parse RFC3339 compliant datetimes. It will not
-- | handle leap seconds until they are supported in `DateTime`.
parseFullDateTime :: forall s m. PS.StringLike s => Monad m => P.ParserT s m FullDateTime
parseFullDateTime = do
  date <- parseDate
  _ <- PC.choice [ PS.char 'T' , PS.char 't' ]
  time <- parsePartialTime
  offset <- parseOffset
  pure $ FullDateTime (DT.DateTime date time) offset

-- | This is a convenience function for the simple use case of `String` to a
-- | `DateTime` or a `ParseError`.
fromString :: String -> Either P.ParseError FullDateTime
fromString s = P.runParser s parseFullDateTime

-- | `toUTC` is a convenience function that will take a `FullDateTime` and use
-- | it's offset to adjust it to a UTC `DateTime`.
toUTC :: FullDateTime -> Maybe DT.DateTime
toUTC (FullDateTime dt Zulu) = pure dt
toUTC (FullDateTime dt (Offset dir hh mm)) = do
  offsetTime <- DT.Time <$> pure hh <*> pure mm <*> toEnum 0 <*> toEnum 0
  zeroTime <- DT.Time <$> toEnum 0 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0
  let duration :: D.Milliseconds
      duration = T.diff zeroTime offsetTime
  case dir of
      Plus -> DT.adjust duration dt
      Minus -> DT.adjust (D.negateDuration duration) dt

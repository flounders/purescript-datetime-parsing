module Test.Main where

import Prelude

import Data.DateTime as DT
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Parsing (ParseError, runParser)

import Data.DateTime.Parsing as P

validZulu :: Either ParseError P.FullDateTime
validZulu = Right <<< unsafePartial fromJust $
  P.FullDateTime <$>
  (DT.DateTime <$>
    (DT.canonicalDate <$> toEnum 1985 <*> pure DT.April <*> toEnum 12) <*>
    (DT.Time <$> toEnum 23 <*> toEnum 20 <*> toEnum 50 <*> toEnum 520)) <*>
  pure P.Zulu

validMinus :: Either ParseError P.FullDateTime
validMinus = Right <<< unsafePartial fromJust $
  P.FullDateTime <$>
  (DT.DateTime <$>
    (DT.canonicalDate <$> toEnum 1996 <*> pure DT.December <*> toEnum 19) <*>
    (DT.Time <$> toEnum 16 <*> toEnum 39 <*> toEnum 57 <*> toEnum 0)) <*>
  (P.Offset <$> pure P.Minus <*> toEnum 8 <*> toEnum 0)

validPlus :: Either ParseError P.FullDateTime
validPlus = Right <<< unsafePartial fromJust $
  P.FullDateTime <$>
  (DT.DateTime <$>
    (DT.canonicalDate <$> toEnum 2003 <*> pure DT.June <*> toEnum 18) <*>
    (DT.Time <$> toEnum 18 <*> toEnum 45 <*> toEnum 34 <*> toEnum 873)) <*>
  (P.Offset <$> pure P.Plus <*> toEnum 2 <*> toEnum 0)

validMillisecond :: Either ParseError P.FullDateTime
validMillisecond = Right <<< unsafePartial fromJust $
  P.FullDateTime <$>
  (DT.DateTime <$>
    (DT.canonicalDate <$> toEnum 2004 <*> pure DT.April <*> toEnum 1) <*>
    (DT.Time <$> toEnum 12 <*> toEnum 20 <*> toEnum 30 <*> toEnum 1)) <*>
  pure P.Zulu

validMillisecond' :: Either ParseError P.FullDateTime
validMillisecond' = Right <<< unsafePartial fromJust $
  P.FullDateTime <$>
  (DT.DateTime <$>
    (DT.canonicalDate <$> toEnum 2004 <*> pure DT.April <*> toEnum 1) <*>
    (DT.Time <$> toEnum 12 <*> toEnum 20 <*> toEnum 30 <*> toEnum 100)) <*>
  pure P.Zulu

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "parser" do
     it "parses zulu" $ do
        let result = runParser "1985-04-12T23:20:50.52Z" P.parseFullDateTime
        result `shouldEqual` validZulu
     it "parses plus/minus offsets" $ do
        let result = runParser "1996-12-19T16:39:57-08:00" P.parseFullDateTime
            result' = runParser "2003-06-18T18:45:34.873+02:00" P.parseFullDateTime
        result `shouldEqual` validMinus
        result' `shouldEqual` validPlus
     it "parses milliseconds" $ do
        let result = runParser "2004-04-01T12:20:30.001Z" P.parseFullDateTime
            result' = runParser "2004-04-01T12:20:30.1Z" P.parseFullDateTime
        result `shouldEqual` validMillisecond
        result' `shouldEqual` validMillisecond'
     it "does not parse leap seconds" $ do
        let result = runParser "1990-12-31T23:59:60Z" P.parseFullDateTime
        case result of
            (Left _) -> pure unit
            _ -> fail "leap seconds seem to work now"
     it "parses only valid dates" $ do
        let result = runParser "2001-02-29T18:00:00Z" P.parseFullDateTime
        case result of
            (Left _) -> pure unit
            _ -> fail "invalid date"

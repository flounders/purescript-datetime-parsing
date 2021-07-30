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
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.Parser (ParseError, runParser)

import Data.DateTime.Parsing as P

validZulu :: Either ParseError P.FullDateTime
validZulu = Right <<< unsafePartial fromJust $
  P.FullDateTime <$>
  (DT.DateTime <$>
    (DT.canonicalDate <$> toEnum 1985 <*> pure DT.April <*> toEnum 12) <*>
    (DT.Time <$> toEnum 23 <*> toEnum 20 <*> toEnum 50 <*> toEnum 520)) <*>
  pure P.Zulu

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "parser" do
     it "parses valid zulu" $ do
        let result = runParser "1985-04-12T23:20:50.52Z" P.parseFullDateTime
        result `shouldEqual` validZulu

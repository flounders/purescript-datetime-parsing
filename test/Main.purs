module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.Parser (parseErrorMessage, runParser)

import Data.DateTime.Parsing as P

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "parsers" do
     it "parses ISO 8601 datetimes" $ do
        let result = runParser "1985-04-12T23:20:50.52Z" P.parseFullDateTime
        case result of
            Left x -> fail $ parseErrorMessage x
            Right _ -> pure unit

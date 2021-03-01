module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Morello.Morello.CompositionSpec as CompositionSpec
import Morello.Morello.MinimalSpec as MinimalSpec
import Morello.Morello.RecordSpec as RecordSpec
--import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  -- specs <- discover "\\..*Spec"
  --runSpec [consoleReporter] specs
  CompositionSpec.spec
  MinimalSpec.spec
  RecordSpec.spec

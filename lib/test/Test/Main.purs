module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config (defaultConfig)

main :: Effect Unit
main = launchAff_ do
  specs <- discover """Test\.PursUtils\..*Spec"""
  liftEffect $ runSpecAndExitProcess'
    { defaultConfig: defaultConfig { timeout = Nothing }
    , parseCLIOptions: true
    }
    [ consoleReporter ]
    specs

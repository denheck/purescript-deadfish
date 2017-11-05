module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(..))
import Deadfish.Interpreter as Deadfish
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = do
  run [consoleReporter] do
    describe "deadfish-lang" do
      it "increments the register by 1" do
        shouldEqual (Deadfish.run "i" 0) $ Tuple "" 1
      it "decrements the register by 1" do
        shouldEqual (Deadfish.run "d" 1) $ Tuple "" 0

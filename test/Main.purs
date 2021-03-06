module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Deadfish.Interpreter (emptyState, withOutput, withRegister)
import Deadfish.Interpreter as Deadfish
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = do
  run [consoleReporter] do
    describe "Interpreter" do
      it "increments the register by 1" do
        shouldEqual (Deadfish.run emptyState "i") $ withRegister emptyState 1
      it "decrements the register by 1" do
        shouldEqual (Deadfish.run (withRegister emptyState 1) "d") $ withRegister emptyState 0
      it "squares the register" do
         shouldEqual (Deadfish.run (withRegister emptyState 2)  "s") $ withRegister emptyState 4
      it "outputs the register" do
        shouldEqual (Deadfish.run (withRegister emptyState 25) "o") $ withOutput (withRegister emptyState 25) "25"
      it "register set to 0 when equal to 256" do
        shouldEqual (Deadfish.run (withRegister emptyState 255) "i") $ emptyState
      it "register set to 0 when equal to -1" do
         shouldEqual (Deadfish.run emptyState "d") $ emptyState
      it "interprets multiple characters" do
         shouldEqual (Deadfish.run emptyState "diissisdo") $ withOutput (withRegister emptyState 288) "288"
      it "ignores characters it can't understand" do
         shouldEqual (Deadfish.run emptyState "qytbx") $ emptyState
      it "outputs character of ascii code in register" do
         shouldEqual (Deadfish.run (withRegister emptyState 75) "c") $ withOutput (withRegister emptyState 75) "K"
      it "loops bracketed instructions 10 times" do
         shouldEqual (Deadfish.run emptyState "iisiiiis{ic}{ic}icicicicicic") $ withOutput (withRegister emptyState 90) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      it "does not execute parenthesized commands when accumulator is 0" do
         shouldEqual (Deadfish.run emptyState "(i)") $ emptyState
      it "won't execute instructions after halt" do
         shouldEqual (Deadfish.run emptyState "hi") $ emptyState
      it "outputs Hello, World! greeting" do
         shouldEqual (Deadfish.run emptyState "w") $ withOutput emptyState "Hello, World!"


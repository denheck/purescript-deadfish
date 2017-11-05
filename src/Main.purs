module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.ST (ST, modifySTRef, newSTRef, readSTRef)
import Deadfish.Interpreter (emptyState, run)
import Node.ReadLine (READLINE, createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)

main :: forall e i. Eff (st :: ST i, readline :: READLINE, exception :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  interface <- createConsoleInterface noCompletion
  ref <- newSTRef emptyState
  setPrompt "> " 2 interface
  prompt interface
  setLineHandler interface $ \input -> do 
    _ <- modifySTRef ref (\state -> run state input)
    state <- readSTRef ref
    logShow state
    prompt interface

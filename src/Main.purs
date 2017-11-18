module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.ST (ST, modifySTRef, newSTRef, readSTRef)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Deadfish.Interpreter (emptyState, run)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.ReadLine (READLINE, createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)
import Node.Yargs.Applicative (flag, runY, yarg)
import Node.Yargs.Setup (example, usage)

app :: forall eff a. Boolean -> String -> Eff (fs :: FS, readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION, st :: ST a | eff) Unit
app false ""     = pure unit -- TODO: should be an error
app false source = (logShow <<< (run emptyState)) =<< readTextFile UTF8 source
app true _  = do
  interface <- createConsoleInterface noCompletion
  ref <- newSTRef emptyState
  setPrompt "> " 2 interface
  prompt interface
  setLineHandler interface $ \input -> do 
       _ <- modifySTRef ref (\state -> run state input)
       state <- readSTRef ref
       logShow state
       prompt interface

main :: forall eff a.  Eff (fs :: FS, readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION, st :: ST a | eff) Unit
main = do
  let setup = usage "$0 --repl" <> example "$0 --repl" "Start the REPL"
  runY setup $ app <$> flag "r" ["repl"] (Just "Start the REPL")  
                   <*> yarg "s" ["source"] (Just "Source file") (Left "") false


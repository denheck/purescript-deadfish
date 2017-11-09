module Deadfish.Interpreter (run, emptyState, withRegister, withOutput) where

import Data.Char (fromCharCode)
import Data.Foldable (foldl)
import Data.String (singleton, toCharArray)
import Data.Tuple (Tuple(..))
import Prelude (show, ($), (*), (+), (-), (<<<), (<>))

type State = Tuple String Int

withOutput :: State -> String -> State
withOutput (Tuple _ register) newOutput =
  Tuple newOutput register 

withRegister :: State -> Int -> State
withRegister (Tuple output _) register =
  Tuple output register

emptyState :: State
emptyState = Tuple "" 0

run :: State -> String -> State
run state input = 
  foldl interpret state (toCharArray input)

interpret :: State -> Char -> State
interpret result char = resetRegister $ interpret' result char
  where
    resetRegister (Tuple output 256) = Tuple output 0
    resetRegister (Tuple output -1) = Tuple output 0
    resetRegister result = result
    interpret' (Tuple output register) 'i' = Tuple output $ register + 1
    interpret' (Tuple output register) 'd' = Tuple output $ register - 1
    interpret' (Tuple output register) 's' = Tuple output $ register * register
    interpret' (Tuple output register) 'o' = Tuple (output <> (show register)) register
    interpret' (Tuple output register) 'c' = Tuple (output <> ((singleton <<< fromCharCode) register)) register
    interpret' result _ = result

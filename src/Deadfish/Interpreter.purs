module Deadfish.Interpreter (run, emptyState, withRegister, withOutput, State) where

import Data.Char (fromCharCode)
import Data.Foldable (foldl)
import Data.Formatter.Internal (repeat)
import Data.String (singleton, toCharArray)
import Deadfish.Lexer (lex, Token(..))
import Prelude (class Eq, class Show, negate, show, ($), (&&), (*), (+), (-), (<<<), (<>), (==))

newtype State = State { output :: String, register :: Int }

instance eqState :: Eq State where 
  eq (State state1) (State state2) = state1.output == state2.output && state1.register == state2.register

instance showState :: Show State where
  show (State state) = "State ('" <> state.output <> "', " <> show state.register <> ")"

withOutput :: State -> String -> State
withOutput (State state) newOutput = State (state { output = newOutput })

withRegister :: State -> Int -> State
withRegister (State state) newRegister = State (state { register = newRegister })

emptyState :: State
emptyState = State { output: "", register: 0 }

run :: State -> String -> State
run state input = 
  foldl interpret state $ lex input
    where
      interpret (State result) char = State (resetRegister $ interpret' result char)
      resetRegister state | state.register == 256 = state { register = 0 }
      resetRegister state | state.register == -1 = state { register = 0 }
      resetRegister state = state
      interpret' state (Command 'i') = state { register = state.register + 1 }
      interpret' state (Command 'd') = state { register = state.register - 1 }
      interpret' state (Command 's') = state { register = state.register * state.register }
      interpret' state (Command 'o') = state { output = state.output <> (show state.register) }
      interpret' state (Command 'c')= state { output = state.output <> ((singleton <<< fromCharCode) state.register) }
      interpret' state (Repeat tokens) = foldl interpret' state $ repeat tokens 10
      interpret' state (Conditional tokens) | state.register == 0 = state
      interpret' state (Conditional tokens) = foldl interpret' state tokens
      interpret' state _ = state

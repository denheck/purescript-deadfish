module Deadfish.Interpreter (run) where

import Data.Foldable (foldl)
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))
import Prelude (($), (+), (-))

type Input = String
type Output = String
type Register = Int

run :: Input -> Register -> Tuple Output Register
run input register = 
  foldl interpret (Tuple "" register) (toCharArray input)

interpret :: Tuple Output Register -> Char -> Tuple Output Register
interpret (Tuple output register) 'i' = Tuple output $ register + 1
interpret (Tuple output register) 'd' = Tuple output $ register - 1
interpret result _ = result

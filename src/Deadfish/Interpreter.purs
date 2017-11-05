module Deadfish.Interpreter (run) where

import Data.Foldable (foldl)
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))
import Prelude (show, ($), (*), (+), (-), (<>))

type Input = String
type Output = String
type Register = Int

run :: Input -> Register -> Tuple Output Register
run input register = 
  foldl interpret (Tuple "" register) (toCharArray input)

interpret :: Tuple Output Register -> Char -> Tuple Output Register
interpret result char = resetRegister $ interpret' result char
  where
    resetRegister (Tuple output 256) = Tuple output 0
    resetRegister (Tuple output -1) = Tuple output 0
    resetRegister result = result
    interpret' (Tuple output register) 'i' = Tuple output $ register + 1
    interpret' (Tuple output register) 'd' = Tuple output $ register - 1
    interpret' (Tuple output register) 's' = Tuple output $ register * register
    interpret' (Tuple output register) 'o' = Tuple (output <> (show register)) register
    interpret' result _ = result

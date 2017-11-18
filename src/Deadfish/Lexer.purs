module Deadfish.Lexer (lex, Token(..)) where

import Data.Array (head, tail)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.String (singleton, toCharArray)
import Data.Tuple (Tuple(..))
import Prelude (class Functor, class Semigroup, class Show, show, (<>))

data Token 
  = Conditional (Array Token)
  | Repeat (Array Token)
  | Command Char

instance showToken :: Show Token where
  show (Command char) = "(Command " <> (singleton char) <> ")"
  show (Repeat tokens) = "(Repeat " <> (show tokens) <> ")"
  show (Conditional tokens) = "(Conditional " <> (show tokens) <> ")"

tailOrEmpty :: forall a. Array a -> Array a
tailOrEmpty arr =
  case tail arr of
    Nothing  -> []
    Just arr -> arr

headAndTail :: forall a. Array a -> Tuple (Maybe a) (Array a)
headAndTail arr = 
  let
    x = head arr
    xs = tailOrEmpty arr
  in
    Tuple x xs

lex :: String -> Array Token
lex characters =
  let
    (Tuple tokens chars) = lex' (Tuple [] (toCharArray characters))
  in
    tokens

lex' :: Tuple (Array Token) (Array Char) -> Tuple (Array Token) (Array Char)
lex' (Tuple tokens characters) = 
  let 
    restCharacters = tailOrEmpty characters
  in
    case head characters of
      Nothing  -> Tuple tokens []
      Just '{' -> 
        let
          (Tuple repeatTokens restCharacters) = lex' (Tuple [] restCharacters)
        in
          lex' (Tuple (tokens <> [(Repeat repeatTokens)]) restCharacters)
      Just '}' -> (Tuple tokens restCharacters)
      Just char -> lex' (Tuple (tokens <> [Command char]) restCharacters)

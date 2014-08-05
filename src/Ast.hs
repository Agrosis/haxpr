module Ast where

data Ast a = Value a | Operation (a -> a -> a) (Ast a) (Ast a)

evaluate :: (Num a) => (Ast a) -> a
evaluate (Value a) = a
evaluate (Operation o a b) = evaluate a `o` evaluate b

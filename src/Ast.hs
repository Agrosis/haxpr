module Ast where

data Ast a = Value a | Operation Op (Ast a) (Ast a)

data Op = Add | Subtract | Multiply | Divide deriving (Show)

evaluate :: (Integral a) => (Ast a) -> a
evaluate (Value a)                = a
evaluate (Operation Add a b)      = evaluate a + evaluate b
evaluate (Operation Subtract a b) = evaluate a - evaluate b
evaluate (Operation Multiply a b) = evaluate a * evaluate b
evaluate (Operation Divide a b)   = evaluate a `div` evaluate b

data AST a = Value a | Operation (a -> a -> a) (AST a) (AST a)

evaluate :: (Num a) => (AST a) -> a
evaluate (Value a) = a
evaluate (Operation o a b) = evaluate a `o` evaluate b

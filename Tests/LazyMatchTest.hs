data Number = Zero | Succ Number
data List (a :: *) = Nil | Cons a (List a)

-- Test has nested Patterns
case (Cons (Succ Zero) (error "tail error")) of
  Cons x xs
    | (Succ Zero || Zero) <- x -> Succ Zero
    | (Succ y)            <- x -> y
  Nil                          -> Zero

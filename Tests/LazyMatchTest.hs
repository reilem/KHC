data Number = Zero | Succ Number
data List (a :: *) = Nil | Cons a (List a)

-- Test has lazy pattern matching with an or and guards with will succeed
case (Cons (Succ Zero) (error "tail error")) of
  Cons x xs
    | (Succ Zero || Zero) <- x -> Succ Zero
    | (Succ y)            <- x -> y
  Nil                          -> Zero

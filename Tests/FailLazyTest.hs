data Number = Zero | Succ Number
data List (a :: *) = Nil | Cons a (List a)

-- Test has lazy pattern matching with an or and guards with will fail
case (Cons (error "head error") Nil) of
  Cons x xs
    | (Succ Zero || Zero) <- x -> Succ Zero
    | (Succ y)            <- x -> y
  Nil                          -> Zero

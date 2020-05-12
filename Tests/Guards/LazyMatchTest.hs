data Number = Zero | Succ Number
data List (a :: *) = Nil | Cons a (List a)

-- TEST USED IN PAPER
-- Test has or-patterns in pattern guards and a diverging input
-- Test should succeed with: Succ Zero
case (Cons (Succ Zero) (error "tail error")) of
  Cons x xs
    | (Succ Zero || Zero) <- x -> Succ Zero
    | (Succ y)            <- x -> y
  Nil                          -> Zero

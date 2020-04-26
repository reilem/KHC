data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has nested Patterns
let countNonZero = \y. case y of
  Nil          -> Zero
  Cons Zero xs -> countNonZero xs
  Cons x    xs -> Succ (countNonZero xs)
in countNonZero (Cons (Succ Zero) (Cons Zero (Cons (Succ Zero) Nil)))

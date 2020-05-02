data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test should fail due to non distint term variables
let length = \y. case y of
  Nil      -> Zero
  Cons x x -> Succ (length xs)
in
length (Cons Zero Nil)

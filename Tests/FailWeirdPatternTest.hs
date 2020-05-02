data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test should fail due to weird pattern
let length = \y. case y of
  x Cons xs -> Succ (length xs)
  Nil      -> Zero
in
length (Cons Zero Nil)

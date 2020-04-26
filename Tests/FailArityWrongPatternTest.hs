data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test should fail due to incorrect arity
let length = \y. case y of
  Cons xs -> Succ (length xs)
  Nil     -> Zero
in
length (Cons Zero Nil)

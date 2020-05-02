data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has incomplete pattern, should result in: Match failed
let foo = \y. case y of
  Nil          -> Zero
in foo (Cons Zero Nil)

data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test should fail due to non distint term variables
\x. let length = \y. case y of
    Cons xs -> Succ (length xs)
    Nil     -> Zero
  in
  length (Cons Zero Nil)

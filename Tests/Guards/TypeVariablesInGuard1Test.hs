data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)
data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Test has guard patterns with type variables in the guard
\x.\y. case x of
    Nil
      | Double a b   <- y -> Zero
      | Triple a b c <- y -> Succ Zero
    Cons x xs
      | Triple a b c <- y -> Zero
      | Quad a b c d <- y -> Succ Zero

data Number = Zero | Succ Number
data List (a :: *) = Nil | Cons a (List a)
data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Test has guard patterns with type variables in the guard
\x.\xs. case x of
  Zero
    | Double (Cons y ys) _ <- Double xs x -> y
    | Double Nil         _ <- Double xs x -> Zero

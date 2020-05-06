data Number = Zero | Succ Number
data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d
data List (a :: *) = Nil | Cons a (List a)

-- Test has an or-pattern that has guards in the rhs
-- GOOD TEST: SHOWS BIG SIZE DIFFERENCE BETWEEN FLATTENING AND LET-BINDINGS OF OR-PATTERNS
case (Cons (Double Zero (Double (Succ (Succ Zero)) Zero)) Nil) of
  Cons x Nil || Cons _ (Cons x Nil)
    | Double _ a <- x, Double b _   <- a -> Succ b
    | Triple _ a _ <- x, Triple b _ _ <- a -> Succ (Succ b)
    | Quad _ a _ _ <- x, Quad b _ _ _ <- a -> Succ (Succ (Succ b))
  Nil -> Zero

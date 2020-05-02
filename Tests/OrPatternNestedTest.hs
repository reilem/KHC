data Result = Zero | One | Two
data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Tests a nested usage of OR patterns. Should return Two.
(\i. case i of
  Double (Triple x _ _ || Quad x _ _ _) y -> x) (Double (Triple Two One One) Zero)

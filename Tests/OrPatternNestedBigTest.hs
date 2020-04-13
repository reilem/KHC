data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Tests a nested usage of OR patterns. Should succeed.
\i. case i of
  Double (Triple x _ _ || Quad x _ _ _) y -> x
  Double (Double _ x || Triple _ x _) y -> x

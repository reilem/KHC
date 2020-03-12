data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Tests a basic usage of OR patterns. Differs from OrPatternTest because
-- the variables x y are now flipped in the right branch.
\i. case i of
  Double x y || Triple y x _ -> x

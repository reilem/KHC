data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Tests a basic usage of OR patterns. Should fail because the variable y
-- is present in the right hand side branch but not in the left one.
\x. case x of
  Double x _ || Triple x y _ -> x

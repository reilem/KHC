data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Tests a basic usage of OR patterns.
\x. case x of
  Double x y || Triple x y _ -> x

data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Tests a basic usage of OR patterns. Should fail because the variable y
-- is present in the left hand side branch but not in the right one and
-- because the variable z is present in the right hand side branch and not
-- the left one.
\x. case x of
  Double x y || Triple x z _ -> x

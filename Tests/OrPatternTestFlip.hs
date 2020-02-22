data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Test uses type constructors with more than two data contructors with
-- more than 2 parameters. Contains incomplete patterns.
\i. case i of
  Double x y || Triple y x _ -> x

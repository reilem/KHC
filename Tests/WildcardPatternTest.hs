data Result = One | Two
data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Test uses type constructors with more than two data contructors with
-- more than 2 parameters. Contains incomplete patterns. Should return Two.
case (Quad Two (Double One One) (Double One One) (Triple One One One)) of
  Double x (Double y y')       -> y'
  Double x _                   -> x
  Triple x y (Triple z z' z'') -> z''
  Quad   x y z (Double w w')   -> w'
  Quad   x y z _               -> x

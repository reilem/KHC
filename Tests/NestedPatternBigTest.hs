data Number = One | Two
data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Test uses type constructors with more than two data contructors with
-- more than 2 parameters. Contains incomplete patterns.
(\i. case i of
  Double x (Double y y')            -> y'
  Double x (Triple y y' y'')        -> y''
  Triple x y (Triple z z' z'')      -> z''
  Quad   x y z (Double w w')        -> w' -- Matches this line, returns Two
  Quad   x y z (Quad w w' w'' w''') -> w'''
) (Quad () (Double One One) (Double One One) (Double One Two))

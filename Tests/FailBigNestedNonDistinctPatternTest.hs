data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Test uses type constructors with more than two data contructors with
-- more than 2 parameters. Contains incomplete patterns and non distinct
-- pattern variables, should error with:
-- [renamer failure] Error: Term variables are not distict in pattern: Triple z z' (Quad x y z a)
case (Double () (Double () ())) of
  Double x (Double y y')                  -> y'
  Double x (Triple y y' y'')              -> y''
  Triple x y (Triple z z' (Quad x y z a)) -> z' -- Double y in this pattern
  Quad   x y z (Double w w')              -> w'
  Quad   x y z (Quad w w' w'' w''')       -> w'''

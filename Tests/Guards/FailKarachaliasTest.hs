data Bool = True | False
data Result = One | Two | Three
data Tuple (a :: *) (b :: *) = Tup a b

-- Test from Karachalias et Al. Should fail with "Expected Failure"
(\x. case x of
  Tup _    False -> One
  Tup True False -> Two
  Tup _    _     -> Three
) (Tup (error "Expected Failure") True)

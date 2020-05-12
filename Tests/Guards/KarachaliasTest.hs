data Bool = True | False
data Result = One | Two | Three
data Tuple (a :: *) (b :: *) = Tup a b

-- Test from Karachalias et Al. Should succeed, and return Three
(\x. case x of
  Tup _    False -> One
  Tup _    _     -> Three
) (Tup (error "Unexpected Failure") True)

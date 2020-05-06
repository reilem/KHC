data Result = One | Two | Three | Four
data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Tests a basic usage of OR patterns. Should return One.
(\x. case x of
  Double x y || Triple x _ y || Quad _ x _ y  -> x
) (Quad Two One Three Two)

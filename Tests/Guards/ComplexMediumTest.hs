data Number = Zero | Succ Number
data Bool = True | False
data List (a :: *) = Nil | Cons a (List a)
data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Test has guard patterns with a nested case-of as guard expression
(\y. case y of
    Nil                                       -> Cons Zero Nil
    Cons x xs
      | Double a b <- case x of
        Zero
          | Cons y ys <- xs, Zero <- y -> Double y y
          | Nil       <- xs            -> Double Zero Zero
        Succ z
          | Zero    <- z -> Double z z
          | Succ zz <- z -> Double zz zz      -> Cons (Succ a) xs
      | Zero <- Zero                          -> Cons Zero xs
) (Cons (Succ (Succ Zero)) (Cons Zero Nil))

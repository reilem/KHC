data Number = Zero | Succ Number
data Bool = True | False
data List (a :: *) = Nil | Cons a (List a)

-- Test has guard patterns with a nested case-of as guard expression
\y. case y of
    Nil                                       -> Cons Zero Nil
    Cons x xs
      | True <- case x of
        Zero
          | Cons y ys <- xs, Zero <- y -> True
          | Nil       <- xs            -> False
        Succ z
          | Zero <- z    -> False
          | Succ zz <- z -> True              -> Cons (Succ x) xs
      | Zero <- Zero                          -> Cons Zero xs

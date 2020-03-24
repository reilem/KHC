data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has guard patterns without bars, should fail
\x. case x of
    Nil       -> Zero
    Cons x xs
      Cons y ys <- x -> y
      Zero <- Zero   -> x

data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has guard patterns and an otherwise
\x. case x of
    Nil       -> Zero
    Cons x xs
      | Cons y ys <- x, Cons y ys <- z_in -> y
      | Zero <- Zero                      -> Zero

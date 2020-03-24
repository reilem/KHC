data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has guard patterns and "nested" guards that should fail
\x. case x of
    Nil       -> Zero
    Cons x xs
      | Cons y ys <- xs -> y
        | Cons y ys <- xy -> y
        | Zero      <- y  -> Zero
      | Zero <- Zero   -> Zero

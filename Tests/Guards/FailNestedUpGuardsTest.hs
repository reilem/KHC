data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has "up nested" guards that should fail
\x. case x of
    Nil       -> Zero
    Cons x xs
      | Cons y ys <- xs -> y
        | Cons z zs <- ys -> z
        | Zero      <- y  -> Zero
      | Zero <- Zero   -> Zero

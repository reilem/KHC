data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has guard patterns with multiple patterns
(\x. case x of
    Nil       -> Zero
    Cons x xs
      | Cons y ys <- x, Cons z zs <- y -> z
      | Zero <- Zero                   -> Zero
) (Cons (Cons (Cons (Succ (Succ Zero)) Nil) Nil) Nil)

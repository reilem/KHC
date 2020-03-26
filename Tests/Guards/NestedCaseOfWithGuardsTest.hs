data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has guard patterns and a nested case expression
\x. case x of
    Nil       -> Zero
    Cons x xs
      | Cons y ys <- xs -> case y of
        Zero   -> Zero
        Succ z -> Succ x
      | Zero <- Zero   -> Zero

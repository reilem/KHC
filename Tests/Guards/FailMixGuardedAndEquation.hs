data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has mixed guard and normal equation, should fail
\x. case x of
    Nil       -> Zero
    Cons x xs
      | Cons y ys <- x -> y
      -> x

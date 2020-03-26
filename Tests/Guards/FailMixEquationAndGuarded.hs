data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has mixed normal equations and guards, should fail
\x. case x of
    Nil       -> Zero
    Cons x xs -> x
      | Cons y ys <- x -> y

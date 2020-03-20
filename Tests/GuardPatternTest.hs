data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has nested Patterns
\x. case x of
    Nil       -> Zero
    Cons x xs
      | Cons y ys <- x -> y
      | otherwise      -> Zero

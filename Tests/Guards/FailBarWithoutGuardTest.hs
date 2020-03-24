data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has bars without guards, should fail
\x. case x of
    Nil       -> Zero
    Cons x xs
      | -> x
      | Zero <- Zero -> x

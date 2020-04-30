data Number = Zero | One | Two | Succ Number
data List (a :: *) = Nil | Cons a (List a)

-- Test has guard patterns and includes an OR pattern
(\x. case x of
  Nil       -> Zero
  Cons x xs
    | Two <- x                      -> One
    | (Zero || One) <- x, Nil <- xs -> Two
    | Zero <- Zero                  -> Zero
) (Cons One Nil)

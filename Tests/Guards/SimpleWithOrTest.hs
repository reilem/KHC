data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Test has guard patterns and includes an OR pattern
(\x. case x of
    Nil       -> Zero
    Cons x xs
      | (Nil || Cons _ _) <- xs -> Succ Zero
      | Zero <- Zero            -> Zero
) (Cons Zero (Cons Zero Nil))

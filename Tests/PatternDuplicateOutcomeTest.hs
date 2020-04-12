data Number = One | Two | Three

data List (a :: *) = Nil | Cons a (List a)

data Triple (a :: *) = Triple a a a

-- Regular patterns test, the outcome should have a section which is duplicated
\i. case i of
    Triple (Cons _ _) _          (Cons _ _) -> One
    Triple _          (Cons _ _) _          -> Two
    Triple _          Nil        _          -> Three

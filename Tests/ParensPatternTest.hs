data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Regular patterns test
let length = \y. case y of
    (Nil)           -> Zero
    (Cons (x) (xs)) -> Succ (length xs)
in length (Cons Zero (Cons Zero (Cons Zero Nil)))

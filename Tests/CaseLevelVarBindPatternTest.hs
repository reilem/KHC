data Number = Zero | Succ Number

data List (a :: *) = Nil | Cons a (List a)

-- Variable binding at first level
let length = \y. case y of
  z -> Cons Zero z
  w -> Cons Zero (Cons Zero w)
in length (Cons Zero Nil)

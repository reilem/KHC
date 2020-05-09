data List   (a :: *) = Nil | Cons a (List a)
data Number = Zero | Succ Number

-- Tests a more complex usage of OR patterns.
-- Should return: Cons (Cons (Succ Zero) Nil) Nil
(\a. case a of
  Cons (Succ x) xs || Cons Zero (Cons x xs) -> Cons (Cons x xs) Nil
  Nil                                       -> Cons Nil Nil
) (Cons Zero (Cons (Succ Zero) Nil))

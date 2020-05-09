data List   (a :: *) = Nil | Cons a (List a)
data Number = Zero | Succ Number

-- Tests a more complex usage of OR patterns.
-- Should return: Cons (Cons (Succ Zero) Nil) Nil
(\a. case a of
  Cons (Succ Zero || Succ (Succ Zero) || Zero) xs -> Cons (Succ Zero) xs
  Cons x                                       xs -> Cons x Nil
  Nil                                             -> Cons Zero Nil
) (Cons Zero (Cons (Succ Zero) Nil))

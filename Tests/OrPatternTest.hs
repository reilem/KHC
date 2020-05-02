data List   (a :: *) = Nil | Cons a (List a)
data Number = Zero | Succ Number

-- Tests a basic usage of OR patterns.
-- Should return: Cons (Cons (Succ Zero) Nil) Nil
(\i. case i of
  Nil                                       -> Cons Nil Nil
  Cons (Succ x) xs || Cons Zero (Cons x xs) -> Cons (Cons x xs) Nil
) (Cons Zero (Cons (Succ Zero) Nil))

data List   (a :: *) = Nil | Cons a (List a)
data Number = Zero | Succ Number

-- Tests a basic usage of OR patterns.
\x. case x of
  Nil                                       -> Cons Nil Nil
  Cons (Succ x) xs || Cons Zero (Cons x xs) -> Cons (Cons x xs) Nil

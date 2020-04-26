data List   (a :: *) = Nil | Cons a (List a)
data Number = Zero | Succ Number

-- Tests a usage of OR patterns. The compiler should output a simplified
-- version because xs subsumes (Cons _ xs). Should return: Cons (Cons Zero Nil) Nil
(\x. case x of
  Nil                                           -> Cons (Cons Zero Nil) Nil
  Cons (Succ x) xs || Cons (Succ x) (Cons _ xs) -> Cons (Cons x xs) Nil
) (Cons (Succ Zero) Nil)

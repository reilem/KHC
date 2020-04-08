data List   (a :: *) = Nil | Cons a (List a)
data Number = Zero | Succ Number

-- Tests a usage of OR patterns. The compiler should output a simplified
-- versions because xs subsumes (Cons _ xs).
\x. case x of
  Nil                                           -> Cons (Cons Zero Nil) Nil
  Cons (Succ x)  || Cons (Succ x) (Cons _ xs) -> Cons (Cons )

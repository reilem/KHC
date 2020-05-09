data List (a :: *) = Nil | Cons a (List a)
data Number        = Zero | Succ Number

-- TEST USED IN PAPER
-- Used to test evaluation order of or-patterns
-- Tests an overlapping or-pattern with an unevaluated error in the input
-- Should succeed and return: Cons Zero Nil
(\i. case i of
  Cons (Succ _ || Succ Zero) xs -> Cons Zero xs
  Cons Zero                  xs -> xs
  Nil                           -> Nil
) (Cons (Succ (error "Unexpected Failure")) Nil)

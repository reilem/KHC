data List (a :: *) = Nil | Cons a (List a)
data Number        = Zero | Succ Number

-- TEST USED IN PAPER
-- Used to test evaluation order of or-patterns
-- Tests an overlapping or-pattern with an evaluated error in the input.
-- Should fail and return: [Evaluation error failure] Expected Failure
(\i. case i of
  Cons (Succ Zero || Succ _) xs -> Cons Zero xs
  Cons Zero                  xs -> xs
  Nil                           -> Nil
) (Cons (Succ (error "Expected Failure")) Nil)

data Number = Zero | Succ Number
data List (a :: *) = Nil | Cons a (List a)

-- TEST USED IN PAPER
-- Contains pattern guards, a nested or-pattern, a wildcard and is incomplete
-- This version should fail with: [Evaluation error failure] Match Failed
case (Cons Zero (Cons Zero (Cons Zero Nil))) of
  Cons x (Nil || Cons _ Nil)
    | Succ y <- x -> Cons y Nil
    | Zero   <- x -> Cons Zero Nil
  Nil             -> Nil

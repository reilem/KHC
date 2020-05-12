data List   (a :: *) = Nil | Cons a (List a)
data Number = Zero | Succ Number

-- Tests a basic usage of OR patterns.
-- Should return: Zero
(\a. case a of
  Succ Zero || Zero -> Zero
  Succ x            -> x) (Succ Zero)

data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d
data Number = Zero | Succ Number

-- Tests a nested usage of OR patterns. Should succeed.
-- GOOD TEST: SHOWS DIFFERENCE BETWEEN OPTIMISED AND UNOPTIMISED COMPILATION OF
-- OR-PATTERNS USING LET-BINDINGS
\i. case i of
  Double (Triple x _ _ || Quad x _ _ _) (Succ y) -> x
  Double (Double _ x || Triple _ x _) Zero -> x

data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

\x.let a = Double x x in x

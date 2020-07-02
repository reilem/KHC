data Number = Zero | Succ Number
data Tuple (a :: *) = Nil
                    | Double a a
                    | Triple a a a
                    | Quad   a a a a

let sumTwoElements = \t. case t of
  Double a b || Triple a _ b || Quad a _ b _
    | Zero     <- a -> b
    | (Succ n) <- a -> Succ (sumTwoElements (Double n b))
  Nil               -> Zero
in sumTwoElements (Triple (Succ (Succ Zero)) Zero (Succ Zero))

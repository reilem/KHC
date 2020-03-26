data Number = Zero | Succ Number
data Bool = True | False
data List (a :: *) = Nil | Cons a (List a)
data Tree (a :: *) = Leaf a | Node (Tree a) (Tree a) | EmptyLeaf

data Tuple (a :: *) (b :: *) (c :: *) (d :: *) = Double a b | Triple a b c | Quad a b c d

-- Test has guard patterns with a nested case-of as guard expression using multiple OR patterns
\x. case x of
    Nil       -> EmptyLeaf
    Cons x xs
    -- Match on: Tuple (Tree Number) Number Number Bool
      | (Double a b || Triple a b _) <- case x of
        Double tree1 tree2 || Triple tree1 tree2 _
          | (Node left1 right1) <- tree1, (Node left2 right2) <- tree2 -> Double (Node left1 right2)
          | (Node left1 right1) <- tree1, (Leaf val2)         <- tree2 -> Triple (Node left1 right1) val2
          | (Leaf val1)         <- tree1, (Node left2 right2) <- tree2 -> Triple (Node left2 right2) val1
          | (Leaf val1)         <- tree1, (Leaf val2)         <- tree2 -> Triple EmptyLeaf val1 val2
        Quad tree1 tree2 bool num
          | False <- bool -> Double tree1 num
          | True  <- bool -> Double tree2 num -> (Node a b)
      | Zero <- Zero            -> Leaf Zero

data Number = Zero | Succ Number

let subOne = \n. case n of
  (Zero || Succ Zero) -> Zero
  (Succ n)            -> n
in subOne (Succ (Succ (Succ (Succ Zero))))

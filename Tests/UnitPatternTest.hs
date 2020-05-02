data Number = Succ Number | Zero

(\i. case i of
  () -> Succ Zero
  () -> Zero) ()

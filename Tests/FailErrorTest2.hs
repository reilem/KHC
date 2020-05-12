data List (a :: *) = Cons a (List a) | Nil

(\x. Cons (error "Expected Failure") Nil) ()

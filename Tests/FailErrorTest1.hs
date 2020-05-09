data Number = Succ Number | Zero

(\x. Succ (error "Expected Failure")) ()

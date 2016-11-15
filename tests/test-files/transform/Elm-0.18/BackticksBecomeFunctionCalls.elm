simple = 1 `f` 2
nested = 1 `f` 2 `g` 3 `h` 4
withSymbolOps = 1 + 2 `f` 3 + 4 `g` 5 + 6
withComments = 1{-A-}`f`{-B-}2{-C-}`g`{-D-}3
needParens = f x y `abc` g x y `def` h x y
withComments2 = f x y{-A-}`abc`{-B-}g x y
withLabmdas =
    a `qq` \x -> f x
      `ww` \y -> g x y

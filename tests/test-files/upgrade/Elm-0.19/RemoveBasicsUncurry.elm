fullyApplied f = uncurry f ("1", 2)
fullyApplied_comments f = uncurry{-A-}f{-B-}({-C-}"1"{-D-},{-E-}2{-F-})
notTuple f t = uncurry f t
notTuple_comments f t = uncurry{-A-}f{-B-}t
functionOnly f = uncurry f
functionOnly_comments f = uncurry{-A-}f
unapplied = uncurry
extraArgs f = uncurry f (1, 2) 3 4 5
extraArgs_notTuple f t u v = uncurry f t u v

qualified f = Basics.uncurry f ("1", 2)

nested f = uncurry f (uncurry f (1, 2), 3)

multiline_fullyApplied f =
    uncurry f
        ( 1
        , 2
        )
multiline_extraArgs_notTuple f t u =
    uncurry f
        t
        u

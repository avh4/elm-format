module ConvertBasicsRem exposing (fullyApplied)

fullyApplied = rem -1 4
fullyApplied_comments = rem{-A-}-1{-B-}4
partiallyApplied = rem 1
unapplied = rem
qualified = Basics.rem 11 4

extraArgs = rem 1 2 3 4

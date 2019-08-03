module RemoveBasicsCurry exposing (fullyApplied)

fullyApplied f = curry f "1" 2
fullyApplied_comments f = curry{-A-}f{-B-}"1"{-C-}2
partiallyApplied f = curry f "1"
partiallyApplied_comments f = curry{-A-}f{-B-}"1"
functionOnly f = curry f
functionOnly_comments f = curry{-A-}f
unapplied = curry
extraArgs f = curry f 1 2 3 4 5

qualified f = Basics.curry f "1" 2

nested f = curry f (curry f 1 2) 3

module RangesBecomeListRange exposing (range)

range = [1..9]
variableRange x y = [x.min..y]
precommented = [{-A-}1..{-B-}2]
precommented2 =
    [ -- One
      1
    .. -- Two
      2
    ]
commented = [{-A-}1{-B-}..{-C-}2{-D-}]
needsParens = [f x..f y]
usedInFunctionCall = f [1..9] [{-A-}1{-B-}..{-C-}2{-D-}]

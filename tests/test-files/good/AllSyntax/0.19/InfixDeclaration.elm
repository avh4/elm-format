module InfixDeclaration exposing ((<<), (<|), (==))

-- INFIX OPERATORS


infix right 0 (<|) = apL
infix non   4 (==) = eq
infix left  9 (<<) = composeL



-- DEFINITIONS


apL f x =
    f x


eq _ _ =
    True


composeL g f x =
    g (f x)



-- declarations close to "infix" should still parse


infi =
    ()


infiz =
    ()


infixy right =
    right

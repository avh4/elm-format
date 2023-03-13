module InfixAsVariableName exposing (..)

{-| Despite being a keyword, "infix" is not a _reserved word_ in Elm 0.19,
and is allowed as a variable name in expressions, type, and patterns.
-}


inExpression infix =
    infix + 1


type InType infix
    = A infix


{-| As top-level declaration
-}
infix x =
    ()


inLetDeclaration =
    let
        infix y =
            ()
    in
    infix

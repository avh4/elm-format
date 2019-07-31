module Test exposing (ExpressionType(..), expressionTypes, typename)


type ExpressionType
    = TrueType
    | FalseType
    | NotType
    | AndType
    | OrType


expressionTypes =
    [ TrueType, FalseType, NotType, AndType, OrType ]


typename : ExpressionType -> String
typename expressionType =
    case expressionType of
        TrueType ->
            "True"

        FalseType ->
            "False"

        NotType ->
            "Not"

        AndType ->
            "And"

        OrType ->
            "Or"

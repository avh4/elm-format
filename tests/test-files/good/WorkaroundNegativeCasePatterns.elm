module MinusErr exposing (errFloats, errInts)


errInts : Int -> Int
errInts x =
    case x of
        (-1) ->
            0

        (-2) ->
            0

        _ ->
            1


errFloats : Float -> Float
errFloats x =
    case x of
        (-1.0) ->
            0

        (-2.1) ->
            0

        _ ->
            1

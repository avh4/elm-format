module Main exposing (bar, multilineList, ratio)


ratio =
    graphHeight
        / (if range == 0 then
            0.1

           else
            toFloat range
          )



-- foo=(case x of {True->1;False->3})


bar =
    if
        if a then
            True

        else
            False
    then
        "a"

    else
        "b"


multilineList =
    [ 1
    , 2
    , 3
    ]

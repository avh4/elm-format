module Main exposing (..)


unaryOperator a =
    -(a - 1) + -2 + -a


multilineUnaryOperator a =
    -(if a then
        1

      else
        2
     )

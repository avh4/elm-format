module AllSyntax.LineComments.Expressions exposing (..)


emptyRecord =
    { --U
    }


record =
    { x =
        --M
        1
        --N
    , --O
      y = 2
    , --Q
      z
      --R
        =
        --S
        3
        --T
    }

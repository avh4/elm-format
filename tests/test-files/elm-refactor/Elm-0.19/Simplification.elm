module Simplification exposing (x)

import A


x a1 =
    [ A.cs A.A
    , A.cs A.B
    , A.varInCase A.First 3 4
    , A.varInCase A.Second 3 4
    , A.varInCase2 A.First 5 6
    , A.varInCase2 A.Second 5 6
    , A.nestedCase A.First A.First 7 8 9
    , A.nestedCase A.First A.Second 7 8 9
    , A.nestedCase A.Second never 7 8 9
    , A.nestedCase a1 A.First 10 never 11
    ]

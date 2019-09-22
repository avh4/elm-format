module UpgradDefinitionSyntax exposing (x)

import A


x =
    [ A.value
    , A.lambda
    , A.lambda 1
    , A.lambda 2 3
    , A.lambda 4 5 6
    , A.function
    , A.function 7
    , A.function 8 9
    , A.function 10 11 12
    ]


cases =
    [ A.caseExhaustive ()
    , A.caseWithPattern Nothing
    , A.caseWithPattern (Just 13)
    , A.caseWithWildcard Nothing
    , A.caseWithWildcard (Just ())
    ]

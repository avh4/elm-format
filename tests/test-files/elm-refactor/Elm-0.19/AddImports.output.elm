module AddImports exposing (x)

import NewQualified
import NewQualified.Aliased as NQA
import Qualified


x =
    [ NewQualified.value
    , NQA.value
    ]


y =
    Qualified.notUpgraded

module Main exposing (x)

import AliasedDifferently as ADIS
import AliasedInSource as AIS


x =
    ()


fromUpgradeDefinition =
    [ A.toAliased
    ]


fromSource =
    [ AIS.div
    , A.toAliasedInSource
    , A.toAliasedDifferentlyInBoth
    ]

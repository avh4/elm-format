module Main exposing (x)

import A
import AliasedDifferently as ADIS
import AliasedInSource as AIS
import AliasedInUpgradeDefinition2


x =
    ()


fromUpgradeDefinition =
    [ A.toAliased
    ]


doesNotApplyNewAliasesToExisitngCode =
    [ AliasedInUpgradeDefinition2.notUpgraded
    ]


fromSource =
    [ AIS.div
    , A.toAliasedInSource
    , A.toAliasedDifferentlyInBoth
    ]

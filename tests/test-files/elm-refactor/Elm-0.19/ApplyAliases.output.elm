module Main exposing (x)

import AliasedDifferently as ADIS
import AliasedInSource as AIS
import AliasedInUpgradeDefinition as AIUD
import AliasedInUpgradeDefinition2


x =
    ()


fromUpgradeDefinition =
    [ AIUD.value
    ]


doesNotApplyNewAliasesToExisitngCode =
    [ AliasedInUpgradeDefinition2.notUpgraded
    ]


fromSource =
    [ AIS.div
    , AIS.value
    , ADIS.value
    ]

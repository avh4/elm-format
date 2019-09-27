module Main exposing (x)

import AliasedDifferently as ADIS
import AliasedInSource as AIS
import AliasedInUpgradeDefinition as AIUD


x =
    ()


fromUpgradeDefinition =
    [ AIUD.value
    ]


fromSource =
    [ AIS.div
    , AIS.value
    , ADIS.value
    ]

module RemoveImports exposing (x)

import Removed
import Removed.Deep
import RemovedAlias as RA
import RemovedAlias.Deep as RAD
import StillUsed
import UnusedButNotRemoved
import UnusedButNotRemoved.Aliased as UBNRA
import UnusedButNotRemoved.Deep


x =
    [ Removed.value
    , Removed.Deep.value
    , RA.value
    , RAD.value
    , StillUsed.value
    , StillUsed.notUpgraded
    ]

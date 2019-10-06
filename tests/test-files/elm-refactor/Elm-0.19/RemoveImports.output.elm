module RemoveImports exposing (x)

import StillUsed
import UnusedButNotRemoved
import UnusedButNotRemoved.Aliased as UBNRA
import UnusedButNotRemoved.Deep


x =
    [ ()
    , ()
    , ()
    , ()
    , ()
    , StillUsed.notUpgraded
    ]

module Matching exposing (qualified)

import Aliased as Ali
import Aliased.Deep as AliDee
import Exposed exposing (exposed1)
import Exposed.Deep exposing (exposed2)
import ExposedAll exposing (..)
import ExposedAll.Deep exposing (..)
import Qualified
import Qualified.Deep


qualified =
    [ ()
    , \a b -> always () (a + b)
    , always () (1 + 2)
    , always ()
    , always () 1 2
    ]


qualifiedDeep =
    [ ()
    , \a b -> always () (a + b)
    , always () (1 + 2)
    , always ()
    , always () 1 2
    ]


aliased =
    [ ()
    , \a b -> always () (a + b)
    , always () (1 + 2)
    , always ()
    , always () 1 2
    ]


noModuleName_doesNotMatch =
    [ noModuleName
    ]


keepFromRemoval =
    -- (removing imports is tested separately)
    [ Ali.notUpgraded
    , Qualified.notUpgraded
    , Qualified.Deep.notUpgraded
    ]

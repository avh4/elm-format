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
    [ Qualified.value
    , Qualified.function
    , Qualified.function 1 2
    , Qualified.curriedFunction
    , Qualified.curriedFunction 1 2
    ]


qualifiedDeep =
    [ Qualified.Deep.value
    , Qualified.Deep.function
    , Qualified.Deep.function 1 2
    , Qualified.Deep.curriedFunction
    , Qualified.Deep.curriedFunction 1 2
    ]


aliased =
    [ Ali.value
    , Ali.function
    , Ali.function 1 2
    , Ali.curriedFunction
    , Ali.curriedFunction 1 2
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

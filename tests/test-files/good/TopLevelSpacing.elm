module Main exposing (Model, class, classList, id)


{ id, class, classList } =
    CssCommon.helpers


type alias Model =
    { mnemonic : String
    , operand : String
    , offset : Int
    }

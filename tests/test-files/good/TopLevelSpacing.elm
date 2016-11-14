module Main exposing (..)


{ id, class, classList } =
    CssCommon.helpers


type alias Model =
    { mnemonic : String
    , operand : String
    , offset : Int
    }

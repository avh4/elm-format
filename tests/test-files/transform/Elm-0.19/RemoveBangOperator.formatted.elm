module Main exposing (..)


batch model a b =
    ( model
    , Cmd.batch [ a, b ]
    )

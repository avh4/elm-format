module Main exposing (..)


batch model a b =
    ( model
    , Cmd.batch [ a, b ]
    )


batch_withComments model a b =
    ( model {- A -}
    , Cmd.batch {- B -} [ a, b ]
    )

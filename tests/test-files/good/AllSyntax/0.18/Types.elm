module AllSyntax.Types exposing (..)

import Dict


unit : ()
unit =
    ()


lambda : () -> () -> ()
lambda _ _ =
    ()


variable : a -> ()
variable _ =
    ()


constructor : Dict.Dict String Int -> ()
constructor _ =
    ()


parens : a -> ()
parens _ =
    ()


tupleFn : (,,) Int String Bool -> ()
tupleFn _ =
    ()


type alias Tuple =
    ( ( a, b )
    , ( {- A -} a {- B -}, {- C -} b {- D -} )
    , ( --A
        a
        --B
      , --C
        b
        --D
      )
    , ( a -- A
      , b -- B
      )
    )


emptyRecord : {} -> ()
emptyRecord _ =
    ()


type alias Record =
    ( { x : Int, y : () }
    , { x : Int -- X
      , y : () -- Y
      }
    )


recordExtension : { a | x : Int, y : Int } -> ()
recordExtension _ =
    ()

module AllSyntaxTypes (..) where

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


tuple : ( a, b ) -> ()
tuple _ =
  ()


emptyRecord : {} -> ()
emptyRecord _ =
  ()


record : { x : Int, y : () } -> ()
record _ =
  ()


recordExtension : { a | x : Int, y : Int } -> ()
recordExtension _ =
  ()

module AllSyntaxTypes (..) where

import Dict


unit : ({- F -})
unit =
  ()


lambda : () -> ()
lambda _ =
  ()


variable : a -> ()
variable _ =
  ()


constructor : Dict.Dict {- A -} String {- B -} Int -> ()
constructor _ =
  ()


tupleFn : (,,) {- C -} Int {- D -} String {- E -} Bool -> ()
tupleFn _ =
  ()


tuple : ( {- G -} a {- H -}, {- I -} b {- J -} ) -> ()
tuple _ =
  ()


emptyRecord : {} -> ()
emptyRecord _ =
  ()


record : { x : Int, y : () } -> ()
record _ =
  ()

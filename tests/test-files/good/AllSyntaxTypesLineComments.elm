module AllSyntaxTypes (..) where

import Dict


unit :
  (-- F
  )
unit =
  ()


lambda : () -> ()
lambda _ =
  ()


variable : a -> ()
variable _ =
  ()


constructor :
  Dict.Dict
    -- A
    String
    -- B
    Int
  -> ()
constructor _ =
  ()


parens :
  (--K
   a
   --L
  )
  -> ()
parens _ =
  ()


tupleFn :
  (,,)
    -- C
    Int
    -- D
    String
    --E
    Bool
  -> ()
tupleFn _ =
  ()


tuple :
  ( --G
    a
    --H
  , --I
    b
    --J
  )
  -> ()
tuple _ =
  ()


emptyRecord :
  { --M
  }
  -> ()
emptyRecord _ =
  ()


record :
  { --N
    x
    --O
      : --P
        Int
        --Q
  , --R
    y
    --S
      : --T
        ()
        --U
  }
  -> ()
record _ =
  ()


recordExtension : { a | x : Int, y : Int } -> ()
recordExtension _ =
  ()

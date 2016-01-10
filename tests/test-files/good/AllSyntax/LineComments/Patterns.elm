module AllSyntax.LineComments.Patterns (..) where


wildcard _ =
  ()


literal 1 2.0 "string" 'c' =
  ()


variable v =
  ()


unit
  (--A
  )
  =
  ()


parens v =
  ()


tuple
  ( --B
    a
    --C
  , --D
    b
    --E
  )
  =
  ()


emptyList
  [--F
  ]
  =
  ()


list
  [ --G
    a
    --H
  , --I
    b
    --J
  ]
  =
  ()


consList (a :: b :: c) =
  ()


record
  { --K
    a
    --L
  , --M
    b
    --N
  }
  =
  ()


asAlias (() as x) =
  ()

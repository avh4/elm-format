module AllSyntax.LineComments.Patterns (..) where


type Foo
  = Foo () ()


wildcard _ =
  ()


literal 1 2.0 "string" 'c' =
  ()


variable v =
  ()


data
  (Foo
    --Q
    x
    --R
    y
  )
  =
  ()


unit
  (--A
  )
  =
  ()


parens
  (--W
   v
   --X
  )
  =
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


consList
  (a
   --T
    :: --U
       b
       --V
    :: --W
       c
  )
  =
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


asAlias
  (()
   --O
   as
    --P
    x
  )
  =
  ()


complexCons (a :: (x :: []) :: ((b :: _) as blah)) =
  ()

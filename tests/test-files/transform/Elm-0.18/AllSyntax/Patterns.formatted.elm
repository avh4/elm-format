module Patterns exposing (consList)


consList ( a :: b :: c, a {- 1 -} :: {- 2 -} b {- 3 -} :: {- 4 -} c, d :: e :: f ) =
    ()


complexCons (a :: (x :: []) :: ((b :: _) as blah)) =
    ()

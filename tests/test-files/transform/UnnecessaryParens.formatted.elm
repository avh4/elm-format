module Main exposing (..)


unnecessaryParens =
    ( "a" ++ f x y ++ "b"
    , f x y
    , x
    , [ 1, 2 ]
    , 1 + (x + 1)
    , { blah = foo << bar
      , stuff =
            if conditional then
                something
            else
                somethingElse
      , thing = doSomething (func arg)
      }
    , if x then
        something
      else
        somethingElse
    , case foo of
        () ->
            ()
    , r.f1.f2.f3.f4
    )


allowedParens =
    ( (x + 1) + (y - 1)
    , ()
    )

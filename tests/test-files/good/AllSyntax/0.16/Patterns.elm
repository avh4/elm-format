module AllSyntax.Patterns (..) where


type Foo
    = Foo () ()


wildcard _ =
    ()


literal 1 2.0 "string" 'c' =
    ()


variable v =
    ()


data (Foo x y) =
    ()


unit () =
    ()


parens v =
    ()


tuple ( a, b ) =
    ()


emptyList [] =
    ()


list [ a, b ] =
    ()


consList (a :: b :: c) =
    ()


record { a, b } =
    ()


asAlias (() as x) =
    ()


complexCons (a :: (x :: []) :: ((b :: _) as blah)) =
    ()

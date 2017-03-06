module AllSyntax.Patterns exposing (..)


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


patternWithQualifiedConstructorAsCosntructorArgument m =
    case m of
        Maybe.Just Maybe.Nothing ->
            ()

        Maybe.Just _ ->
            ()

        Maybe.Nothing ->
            ()


patternWithUnqualifiedConstructorAsCosntructorArgument m =
    case m of
        Maybe.Just Nothing ->
            ()

        (Just _) as x ->
            ()

        Nothing as x ->
            ()

        (Maybe.Nothing as y) as x ->
            ()

        _ ->
            ()

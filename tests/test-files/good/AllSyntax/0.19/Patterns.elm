module AllSyntax.Patterns exposing (unit)


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


consList
    ( a :: b :: c
    , a {- 1 -} :: {- 2 -} b {- 3 -} :: {- 4 -} c
    , --0
      d --1
        :: --2
           e
        --3
        :: --4
           f
      --5
    , d
        --E
        :: e
        --F
        :: f
    , d --D
        :: e --E
        :: f --F
    )
    =
    ()


complexCons (a :: (x :: []) :: ((b :: _) as blah)) =
    ()


record { a, b } =
    ()


emptyRecord {}
    {{- 1 -}}
    { --2
    }
    =
    ()


asAlias (() as x) =
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

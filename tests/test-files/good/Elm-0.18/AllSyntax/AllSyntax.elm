module AllSyntax exposing
    ( fn
    , Data(A, B, C), MultilineData(..), Type, tuple
    )

{-| An example of all valid Elm syntax.


# Section

@docs fn

-}

import Json.Decode as Json
import List exposing (..)
import Signal exposing (foldp, map)
import String
import Task


type Data x y z
    = A
    | B Int
    | C (List Int)
    | D { f1 : Bool, f2 : x }
    | E (y -> z)


type MultilineData
    = MultilineData
        { f1 : ()
        , f2 : Int -> Float
        }


type Data2
    = Foo
    | Too
    | Bar


type alias Type =
    String


type alias TypeWithArgs a b c =
    List ( a, b, { field : c } )


type alias MoreTypes x y z =
    { x | field : y, rec : { z : z }, fn : y -> String -> x }


type alias ParensInTypes a b c =
    { f1 : a -> b
    , f2 : (a -> b) -> c
    , f3 : c -> List (a -> b)
    , f4 : List (List (List b))
    , f5 : List Type
    , f6 : ( a -> b, List (List c) )
    , f7 : List Type -> (a -> List b) -> List c
    }


type alias MultilineRecordType =
    { x : Int
    , y : Int
    , z : Int
    }


type alias MultilineRecordExtension a =
    { a
        | b : Bool
        , c : Char
    }


type alias NestedRecords a =
    { f1 : Int
    , f2 : { singleLine : () }
    , f3 :
        { multiline1 : ( (), () )
        , multiline2 : { inner : List Char }
        , multiline3 :
            { a
                | multiline' : Bool
            }
        }
    , f4 :
        { single : {} }
    , f5 :
        MoreTypes Int Float Bool
    }


{-| A function.
-}
fn =
    "XYZZY"


annotatedFn : String
annotatedFn =
    "XYZZY"


fn2 :
    { x : Int
    , y : String
    }
    -> String
    ->
        { a
            | x : Float
        }
    -> Int
fn2 _ _ _ =
    999


inlinePipeline =
    1 |> (+) 2


tuple =
    ( 1, 2 )


tupleFunction =
    (,,,) 1 2 3 4


multilineTuple a b =
    ( 1
    , if b then
        2

      else
        3
    , (+) 3 4
    , 7
        + 9
    )


vars =
    ( Foo
    , Too
    , Bar
    )


lists =
    ( [ 1, 2, 3, 4 ], [] )


multilineLists =
    [ "one"
    , "two"
    , "three"
    ]


nestedMultilineLists =
    [ []
    , [ [], [ "1" ] ]
    , [ [ "a"
        , "b"
        ]
      ]
    , [ [] ]
    ]


functionCallInMultilineList =
    [ [ [ toString "a" ] ]
    , [ [ toString
            -- A
            "a"
        ]
      ]
    ]


commentedLiterals =
    ( {- int -} 1, {- float -} 0.1, {- char -} 'c', {- string -} "str", {- boolean -} True )


functionApplication =
    toString 10


commentedFunctionApplication =
    toString {- arg1 -} 10


multilineFunctionApplication =
    List.map toString
        [ 1, 2, 3 ]


functionWithParam a =
    a


functionParameters a b ( t, s, _, ( t', s', _, ( t'', s'' ), { x', y' } ) ) { x, y } _ =
    ()


patternAlias ({ x, y } as r) ( a, { b } as r' ) =
    r.x == y


fnAsLambda =
    \a -> a


multiArgLambda =
    \a b ( t, s, _, ( t', s', _, ( t'', s'' ), { x', y' } ) ) { x, y } _ -> \c -> \d -> ()


multilineLambda =
    \a ->
        \b c ->
            \d -> e


parenthesizedExpressions =
    1 + (2 * 3) / 4 |> (+) 0


multilineParenthesizedExpressions graphHeight range =
    graphHeight
        / (if range == 0 then
            0.1

           else
            toFloat range
          )
        ==/==
            (if range == 0 then
                0.2

             else
                toFloat (range - 1)
            )
        <<>>
            (if range == 0 then
                -1.0

             else
                0.0
            )


multilineParenthesizedExpressions2 range arg =
    (if range == 0 then
        always 0.1

     else
        toFloat range
    )
        arg


recordAccess r =
    r.f1


recordAccessAsFunction r =
    .f1 r


multilineRecordAccess f =
    (True
        |> f
    ).f1


multilineRecordUpdate r f =
    { r
        | f1 = 1
        , f2 = 2
    }.f2


multilineRecordAccess2 r f =
    { f1 = 1
    , f2 = 2
    }.f2


chainedRecordAccess r =
    (r.f1.f2.f3.f4
        -- A
        ()
    ).f5.f6


multilineRecordLiteral =
    { f1 = ()
    , f2 = 2
    }


singleLineRecord addStatus =
    addStatus { f1 = 50 }.f1


singleLineRecordUpdate x =
    always { x | f1 = 20 }.f1


letExpression =
    let
        x =
            1
    in
    x


multilineDeclarationInLet =
    let
        string =
            "String"

        string' =
            "String Prime"
    in
    string


ifStatement b =
    if b == "y" then
        "YES"

    else if b == "Y" then
        "yes"

    else
        "No"


caseStatement mb =
    case mb of
        Just True ->
            "+"

        Just _ ->
            "_"

        Nothing ->
            "."


multilineExpressionsInsideList =
    [ let
        x =
            1
      in
      always x
    , if True then
        always 2

      else if False then
        always 3

      else
        always 4
    , case True of
        _ ->
            always 5
    , [ 6
      , 7
      ]
        |> head
        |> Maybe.withDefault 8
        |> always
    , \a ->
        9
    ]


multilineExpressionsInsideTuple a foo =
    ( let
        x =
            1
      in
      x
    , if True then
        2

      else if False then
        3

      else
        4
    , case True of
        _ ->
            5
    , [ 6
      , 7
      ]
        |> head
        |> Maybe.withDefault 8
    , \a ->
        9
    , foo 17
        10
    , ( 11
      , 12
      )
    , { x = 13
      , y = 14
      }
    , { a
        | x = 15
        , y = 16
      }
    )


multilineExpressionsInsideRecord =
    { a =
        let
            x =
                1
        in
        x
    , b =
        if True then
            2

        else if False then
            3

        else
            4
    , c =
        case True of
            _ ->
                5
    , d =
        [ 6
        , 7
        ]
            |> head
            |> Maybe.withDefault 8
    , e =
        \a ->
            9
    }


multilineIfCondition a b =
    if
        if a == Nothing then
            True

        else
            False
    then
        "Yes"

    else if
        if b == Nothing then
            True

        else
            False
    then
        "Perhaps"

    else
        "No"


multilineCaseSubject a =
    case
        if a == Nothing then
            "X"

        else
            "Y"
    of
        _ ->
            ()


singleLineRange =
    [{ f1 = 6 }.f1..(9 + 6 |> (-) 2) + 2]


multilineRange =
    [
        if True then
            1

        else
            2
    ..
        if False then
            3

        else
            5
    ]


nestedMultilineRange =
    [ [
        if True then
            1

        else
            2
      ..
        if False then
            3

        else
            5
      ]
    , [4..2]
    ]


indentedMultilineInsideMultilineInfixApplication div id =
    div [ id "page" ]
        @ { x = 1
          , y = 2
          }
        ++ { x = 1
           , y = 2
           }
        *** { x = 1
            , y = 2
            }
        <<>>
            { x = 1
            , y = 2
            }
        ==/==
            { x = 1
            , y = 2
            }


port runner : Signal (Task.Task x ())
port runner =
    Signal.constant (Task.succeed ())


infixl 4 |.
infix 1 <>
infixr 8 <<>>
infixr 9 ==/==


(|.) : Data a b z -> Data b c z -> Data a b z
(|.) =
    always


(<>) =
    always


(<<>>) =
    always


(==/==) =
    always


(***) =
    always


(@) =
    always


port portFollowedByAnnotation : Signal Int


somethingAfterIncomingPort : ()
somethingAfterIncomingPort =
    ()

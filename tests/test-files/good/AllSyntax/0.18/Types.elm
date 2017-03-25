module AllSyntax.Types exposing (..)

import Dict exposing (Dict)


type alias Unit =
    ( ()
    , ({- A -})
    , (--B
      )
    )


type alias Function =
    ( () -> () -> ()
    , ()
      -> ()
      -> ()
    , (--
      )
      ->
        (--
        )
      ->
        (--
        )
    , () {- AF -} -> {- AG -} () {- AH -} -> {- AI -} ()
    , () --A
      --B
      ->
        --C
        ()
      --D
      ->
        --G
        ()
    , () --A
      -> () --B
      -> () --C
    )


type alias Variable a =
    a


type alias Constructor =
    ( Bool
    , Dict String Int
    , Dict.Dict String Int
    , Dict.Dict {- A -} String {- B -} Int
    , Dict.Dict
        -- A
        String
        -- B
        Int
    )


type alias Parens =
    ( ({- A -} Int {- B -})
    , (--A
       Int
       --B
      )
    )


type alias TupleConstructor =
    ( (,) Int String
    , (,,) Int String Bool
    , (,,) {- C -} Int {- D -} String {- E -} Bool
    , (,,)
        -- C
        Int
        -- D
        String
        --E
        Bool
    )


type alias Tuple =
    ( ( a, b )
    , ( {- A -} a {- B -}, {- C -} b {- D -} )
    , ( --A
        a
        --B
      , --C
        b
        --D
      )
    , ( a -- A
      , b -- B
      )
    )


type alias TupleWithCommentedOutTerms =
    ( Int
      -- , Int
      -- , Int
    , Int
      -- , Int
    )


emptyRecord : {} -> ()
emptyRecord _ =
    ()


type alias Record =
    ( { x : Int, y : () }
    , { x : Int -- X
      , y : () -- Y
      }
    )


recordExtension : { a | x : Int, y : Int } -> ()
recordExtension _ =
    ()

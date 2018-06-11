module AllSyntax.Declarations exposing (DataType1(..), DataType2(..), DataType3(..), DataTypeWithCommentedOutConstructors(..), DataTypeWithEolComments1(..), DataTypeWithEolComments2(..), DataTypeWithParams1(..), DataTypeWithParams2(..), DataTypeWithParams3(..), TypeAlias1, TypeAlias2, TypeAlias3, TypeAliasMultiline, expressionDefinition, expressionDefinitionNoPatterns, expressionDefinitionsInLet, expressionDefinitionsInLet2, expressionDefinitionsInLet3, expressionTypeAnnotation1, expressionTypeAnnotation2, expressionTypeAnnotation3, expressionTypeAnnotation4, expressionTypeAnnotationForcedMultiline)

--
-- Union type
--


type DataType1
    = Ctor1
    | Ctor2 ()
    | Ctor3 (List ()) ()


type {- A -} DataType2 {- B -}
    = {- C -} Ctor1 {- D -}
    | {- E -} Ctor2 {- F -} () {- G -}
    | {- H -} Ctor3 {- I -} (List ()) {- J -} ()


type
    --A
    DataType3
    --B
    = --C
      Ctor1
      --D
    | --E
      Ctor2
        --F
        ()
      --G
    | --H
      Ctor3
        --I
        (List ())
        --J
        ()


type DataTypeWithEolComments1
    = Ctor1 --A


type DataTypeWithEolComments2
    = Ctor1 --A
    | Ctor2 a --B
    | Ctor3 b c --C


type DataTypeWithCommentedOutConstructors
    = A
      -- | B
      -- | C
    | D
    | E



--
-- Union type with params
--


type DataTypeWithParams1 a b c
    = Ctor1'
    | Ctor2' a
    | Ctor3' b c


type {- K -} DataTypeWithParams2 {- L -} a {- M -} b {- N -} c {- O -}
    = Ctor1'
    | Ctor2' a
    | Ctor3' b c


type
    --K
    DataTypeWithParams3
        --L
        a
        --M
        b
        --N
        c
    --O
    = Ctor1'
    | Ctor2' a
    | Ctor3' b c



--
-- Type alias
--


type alias TypeAlias1 a b =
    { x : Int, y : Int, z : ( a, b ) }


type alias
    TypeAliasMultiline
        -- A
        a
        b
    =
    ()


type {- P -} alias {- Q -} TypeAlias2 {- R -} a {- S -} b {- T -} =
    {- U -}
    { x : Int, y : Int, z : ( a, b ) }


type
    --P
    alias
    --Q
    TypeAlias3
        --R
        a
        --S
        b
    --T
    =
    --U
    { x : Int, y : Int, z : ( a, b ) }



--
-- Expressions
--


expressionDefinition _ _ =
    ()


expressionDefinitionNoPatterns =
    ()


expressionTypeAnnotation1 : ()
expressionTypeAnnotation1 =
    ()


expressionTypeAnnotation2 {- V -} : {- W -} ()
expressionTypeAnnotation2 =
    ()


expressionTypeAnnotation3
--V
    :
    --W
    ()
expressionTypeAnnotation3 =
    ()


expressionTypeAnnotation4 :
    --AD
    ()
expressionTypeAnnotation4 =
    ()


expressionTypeAnnotationForcedMultiline :
    Int
    -> Result String Bool
    -> List (Maybe ())
expressionTypeAnnotationForcedMultiline _ _ =
    []



--
-- Declarations in let expressions
--


expressionDefinitionsInLet =
    let
        def1 =
            ()

        def2 =
            ()
    in
    ()


expressionDefinitionsInLet2 =
    let
        {- X -}
        def1 =
            {- Z -}
            ()

        {- AA -}
        def2 =
            {- AC -}
            ()

        {- AD -}
    in
    {- AE -}
    ()


expressionDefinitionsInLet3 =
    let
        --X
        def1 =
            --Y
            ()

        --Z
        def2 =
            --AA
            ()

        --AB
    in
    --AC
    ()

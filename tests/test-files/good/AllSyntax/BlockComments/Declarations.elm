module AllSyntax.BlockComments.Declarations (..) where


type {- A -} DataType {- B -}
  = {- C -} Ctor1 {- D -}
  | {- E -} Ctor2 {- F -} () {- G -}
  | {- H -} Ctor3 {- I -} (List ()) {- J -} ()


type {- K -} DataTypeWithParams {- L -} a {- M -} b {- N -} c {- O -}
  = Ctor1'
  | Ctor2' a
  | Ctor3' b c


type alias TypeAlias =
  { x : Int, y : Int }


expressionDefinition _ _ =
  ()


expressionDefinitionNoPatterns =
  ()


expressionDefinitionsInLet =
  let
    def1 =
      ()

    def2 =
      ()
  in
    ()


expressionTypeAnnotation : ()
expressionTypeAnnotation =
  ()

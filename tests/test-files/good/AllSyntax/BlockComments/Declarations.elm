module AllSyntax.BlockComments.Declarations (..) where


type {- A -} DataType {- B -}
  = {- C -} Ctor1 {- D -}
  | {- E -} Ctor2 {- F -} () {- G -}
  | {- H -} Ctor3 {- I -} (List ()) {- J -} ()


type {- K -} DataTypeWithParams {- L -} a {- M -} b {- N -} c {- O -}
  = Ctor1'
  | Ctor2' a
  | Ctor3' b c


type {- P -} alias {- Q -} TypeAlias {- R -} a {- S -} b {- T -} =
  {- U -}
  { x : Int, y : Int, z : ( a, b ) }


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


expressionTypeAnnotation {- V -} : {- W -} ()
expressionTypeAnnotation =
  ()

module AllSyntax.Declarations (..) where


type DataType
  = Ctor1
  | Ctor2 ()
  | Ctor3 (List ()) ()


type DataTypeWithParams a b c
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

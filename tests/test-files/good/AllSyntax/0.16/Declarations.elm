module AllSyntax.Declarations (..) where


type DataType
  = Ctor1
  | Ctor2 ()
  | Ctor3 (List ()) ()


type DataTypeWithParams a b c
  = Ctor1'
  | Ctor2' a
  | Ctor3' b c


type alias TypeAlias a b =
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


expressionTypeAnnotation : ()
expressionTypeAnnotation =
  ()


expressionTypeAnnotationForcedMultiline :
  Int
  -> Result String Bool
  -> List (Maybe ())
expressionTypeAnnotationForcedMultiline _ _ =
  []

module Parse.Declaration where

import Parse.ParsecAdapter ( (<|>), (<?>), choice, digit, optionMaybe, string, try )

import AST.Structure
import qualified Data.Indexed as I
import ElmVersion
import Parse.Comments
import qualified Parse.Expression as Expr
import Parse.Helpers as Help
import qualified Parse.Type as Type
import AST.V0_16
import Parse.IParser
import Parse.Whitespace
import Reporting.Annotation (Located)


declaration :: ElmVersion -> IParser (I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'TopLevelDeclarationNK)
declaration elmVersion =
    typeDecl elmVersion <|> infixDecl elmVersion <|> port elmVersion <|> definition elmVersion


topLevelStructure :: IParser a -> IParser (TopLevelStructure a)
topLevelStructure entry =
    choice
        [ DocComment <$> docCommentAsMarkdown
        , Entry <$> entry
        ]



-- TYPE ANNOTATIONS and DEFINITIONS

definition :: ElmVersion -> IParser (I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'TopLevelDeclarationNK)
definition elmVersion =
    fmap I.Fix2 $ addLocation $ fmap (CommonDeclaration . I.Fix2) $ addLocation
    (
        (Expr.typeAnnotation elmVersion TypeAnnotation
            <|> Expr.definition elmVersion Definition
        )
        <?> "a value definition"
    )


-- TYPE ALIAS and UNION TYPES

typeDecl :: ElmVersion -> IParser (I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'TopLevelDeclarationNK)
typeDecl elmVersion =
  fmap I.Fix2 $ addLocation $
  do  try (reserved elmVersion "type") <?> "a type declaration"
      postType <- forcedWS
      isAlias <- optionMaybe (string "alias" >> forcedWS)

      name <- capVar elmVersion
      args <- spacePrefix (lowVar elmVersion)
      (C (preEquals, postEquals) _) <- padded equals
      let nameWithArgs = NameWithArgs name args

      case isAlias of
        Just postAlias ->
            do  tipe <- Type.expr elmVersion <?> "a type"
                return $
                  TypeAlias
                    postType
                    (C (postAlias, preEquals) nameWithArgs)
                    (C postEquals tipe)

        Nothing ->
            do
                tags_ <- pipeSep1 (Type.tag elmVersion) <?> "a constructor for a union type"
                return
                    Datatype
                        { nameWithArgs = C (postType, preEquals) nameWithArgs
                        , tags = exposedToOpen postEquals tags_
                        }


-- INFIX


infixDecl :: ElmVersion -> IParser (I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'TopLevelDeclarationNK)
infixDecl elmVersion =
    expecting "an infix declaration" $
    choice
        [ try $ infixDecl_0_16 elmVersion
        , infixDecl_0_19 elmVersion
        ]


infixDecl_0_19 :: ElmVersion -> IParser (I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'TopLevelDeclarationNK)
infixDecl_0_19 elmVersion =
    fmap I.Fix2 $ addLocation $
    let
        assoc =
            choice
                [ string "right" >> return R
                , string "non" >> return N
                , string "left" >> return L
                ]
    in
    Fixity
        <$> (try (reserved elmVersion "infix") *> preCommented assoc)
        <*> preCommented ((\n -> read [n]) <$> digit)
        <*> commented symOpInParens
        <*> (equals *> preCommented (lowVar elmVersion))


infixDecl_0_16 :: ElmVersion -> IParser (I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'TopLevelDeclarationNK)
infixDecl_0_16 elmVersion =
  fmap I.Fix2 $ addLocation $
  do  assoc <-
          choice
            [ try (reserved elmVersion "infixl") >> return L
            , try (reserved elmVersion "infixr") >> return R
            , try (reserved elmVersion "infix")  >> return N
            ]
      digitComments <- forcedWS
      n <- digit
      opComments <- forcedWS
      Fixity_until_0_18 assoc digitComments (read [n]) opComments . I.Fix2 <$> addLocation (VarRef_ <$> anyOp elmVersion)


-- PORT

port :: ElmVersion -> IParser (I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'TopLevelDeclarationNK)
port elmVersion =
  expecting "a port declaration" $
  fmap I.Fix2 $ addLocation $
  do  try (reserved elmVersion "port")
      preNameComments <- whitespace
      name <- lowVar elmVersion
      postNameComments <- whitespace
      let name' = C (preNameComments, postNameComments) name
      choice [ portAnnotation name', portDefinition name' ]
  where
    portAnnotation name =
      do  try hasType
          typeComments <- whitespace
          tipe <- Type.expr elmVersion <?> "a type"
          return (PortAnnotation name typeComments tipe)

    portDefinition name =
      do  try equals
          bodyComments <- whitespace
          expr <- Expr.expr elmVersion
          return (PortDefinition_until_0_16 name bodyComments expr)

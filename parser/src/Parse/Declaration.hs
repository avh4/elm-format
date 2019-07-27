{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Declaration where

import Text.Parsec ( (<|>), (<?>), choice, digit, optionMaybe, string, try )

import AST.Declaration
import ElmVersion
import Parse.Comments
import qualified Parse.Expression as Expr
import Parse.Helpers as Help
import qualified Parse.Type as Type
import AST.V0_16
import Parse.IParser
import Parse.Whitespace


declaration :: ElmVersion -> IParser Declaration
declaration elmVersion =
    typeDecl elmVersion <|> infixDecl elmVersion <|> port elmVersion <|> definition elmVersion


topLevelStructure :: IParser a -> IParser (TopLevelStructure a)
topLevelStructure entry =
    choice
        [ DocComment <$> docCommentAsMarkdown
        , Entry <$> addLocation entry
        ]



-- TYPE ANNOTATIONS and DEFINITIONS

definition :: ElmVersion -> IParser AST.Declaration.Declaration
definition elmVersion =
    (Expr.typeAnnotation elmVersion TypeAnnotation <|> Expr.definition elmVersion Definition)
    <?> "a value definition"


-- TYPE ALIAS and UNION TYPES

typeDecl :: ElmVersion -> IParser AST.Declaration.Declaration
typeDecl elmVersion =
  do  try (reserved elmVersion "type") <?> "a type declaration"
      postType <- forcedWS
      isAlias <- optionMaybe (string "alias" >> forcedWS)

      name <- capVar elmVersion
      args <- spacePrefix (lowVar elmVersion)
      (preEquals, _, postEquals) <- padded equals

      case isAlias of
        Just postAlias ->
            do  tipe <- Type.expr elmVersion <?> "a type"
                return $
                  AST.Declaration.TypeAlias
                    postType
                    (Commented postAlias (name, args) preEquals)
                    (postEquals, tipe)

        Nothing ->
            do
                tags_ <- pipeSep1 (Type.tag elmVersion) <?> "a constructor for a union type"
                return
                    AST.Declaration.Datatype
                        { nameWithArgs = Commented postType (name, args) preEquals
                        , tags = exposedToOpen postEquals tags_
                        }


-- INFIX


infixDecl :: ElmVersion -> IParser AST.Declaration.Declaration
infixDecl elmVersion =
    expecting "an infix declaration" $
    choice
        [ try $ infixDecl_0_16 elmVersion
        , infixDecl_0_19 elmVersion
        ]


infixDecl_0_19 :: ElmVersion -> IParser AST.Declaration.Declaration
infixDecl_0_19 elmVersion =
    let
        assoc =
            choice
                [ string "right" >> return AST.Declaration.R
                , string "non" >> return AST.Declaration.N
                , string "left" >> return AST.Declaration.L
                ]
    in
    AST.Declaration.Fixity_0_19
        <$> (try (reserved elmVersion "infix") *> preCommented assoc)
        <*> (preCommented $ (\n -> read [n]) <$> digit)
        <*> (commented symOpInParens)
        <*> (equals *> preCommented (lowVar elmVersion))


infixDecl_0_16 :: ElmVersion -> IParser AST.Declaration.Declaration
infixDecl_0_16 elmVersion =
  do  assoc <-
          choice
            [ try (reserved elmVersion "infixl") >> return AST.Declaration.L
            , try (reserved elmVersion "infixr") >> return AST.Declaration.R
            , try (reserved elmVersion "infix")  >> return AST.Declaration.N
            ]
      digitComments <- forcedWS
      n <- digit
      opComments <- forcedWS
      AST.Declaration.Fixity assoc digitComments (read [n]) opComments <$> anyOp elmVersion


-- PORT

port :: ElmVersion -> IParser AST.Declaration.Declaration
port elmVersion =
  expecting "a port declaration" $
  do  try (reserved elmVersion "port")
      preNameComments <- whitespace
      name <- lowVar elmVersion
      postNameComments <- whitespace
      let name' = Commented preNameComments name postNameComments
      choice [ portAnnotation name', portDefinition name' ]
  where
    portAnnotation name =
      do  try hasType
          typeComments <- whitespace
          tipe <- Type.expr elmVersion <?> "a type"
          return (AST.Declaration.PortAnnotation name typeComments tipe)

    portDefinition name =
      do  try equals
          bodyComments <- whitespace
          expr <- Expr.expr elmVersion
          return (AST.Declaration.PortDefinition name bodyComments expr)

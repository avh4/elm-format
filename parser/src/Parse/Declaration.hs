{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Declaration where

import Text.Parsec ( (<|>), (<?>), choice, digit, optionMaybe, string, try )

import qualified AST.Declaration
import qualified Parse.Expression as Expr
import Parse.Helpers as Help
import qualified Parse.Type as Type
import AST.V0_15


declaration :: IParser AST.Declaration.Decl
declaration =
  choice
    [ AST.Declaration.DocComment <$> Help.docComment
    , AST.Declaration.Decl <$> addLocation (typeDecl <|> infixDecl <|> port <|> definition)
    ]


-- TYPE ANNOTATIONS and DEFINITIONS

definition :: IParser AST.Declaration.Declaration
definition =
  AST.Declaration.Definition
    <$> (Expr.typeAnnotation <|> Expr.definition)
    <?> "a value definition"


-- TYPE ALIAS and UNION TYPES

typeDecl :: IParser AST.Declaration.Declaration
typeDecl =
  do  try (reserved "type") <?> "a type declaration"
      forcedWS
      isAlias <- optionMaybe (string "alias" >> forcedWS)

      name <- capVar
      args <- map (\(Commented _ _ v) -> v) <$> spacePrefix lowVar -- TODO: use comments
      padded equals

      case isAlias of
        Just _ ->
            do  tipe <- Type.expr <?> "a type"
                return (AST.Declaration.TypeAlias name args tipe)

        Nothing ->
            do  tcs <- pipeSep1 (const . const <$> Type.constructor) <?> "a constructor for a union type" -- TODO: use comments
                return $ AST.Declaration.Datatype name args (tcs [] []) -- TODO: pass comments


-- INFIX

infixDecl :: IParser AST.Declaration.Declaration
infixDecl =
  expecting "an infix declaration" $
  do  assoc <-
          choice
            [ try (reserved "infixl") >> return AST.Declaration.L
            , try (reserved "infixr") >> return AST.Declaration.R
            , try (reserved "infix")  >> return AST.Declaration.N
            ]
      forcedWS
      n <- digit
      forcedWS
      AST.Declaration.Fixity assoc (read [n]) <$> anyOp


-- PORT

port :: IParser AST.Declaration.Declaration
port =
  expecting "a port declaration" $
  do  try (reserved "port")
      whitespace
      name <- lowVar
      whitespace
      choice [ portAnnotation name, portDefinition name ]
  where
    portAnnotation name =
      do  try hasType
          whitespace
          tipe <- Type.expr <?> "a type"
          return (AST.Declaration.PortAnnotation name tipe)

    portDefinition name =
      do  try equals
          whitespace
          expr <- Expr.expr
          return (AST.Declaration.PortDefinition name expr)

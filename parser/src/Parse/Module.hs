module Parse.Module (moduleDecl, elmModule) where

import Text.Parsec hiding (newline, spaces)

import Parse.Helpers
import Parse.Declaration as Decl
import qualified AST.Declaration
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import Reporting.Annotation as A hiding (map)
import AST.V0_16


elmModule :: IParser Module.Module
elmModule =
  do  h <- header
      decls <- declarations
      trailingComments <-
          (++)
              <$> option [] freshLine
              <*> option [] spaces
      eof

      return $ Module.Module h (decls ++ (map AST.Declaration.BodyComment trailingComments))


declarations :: IParser [AST.Declaration.Decl]
declarations =
  (++) <$> ((\x -> [x]) <$> Decl.declaration) -- TODO: can there be comments before this?
      <*> (concat <$> many freshDef)


freshDef :: IParser [AST.Declaration.Decl]
freshDef =
    commitIf (freshLine >> (letter <|> char '_')) $
      do  comments <- freshLine
          decl <- Decl.declaration
          return $ (map AST.Declaration.BodyComment comments) ++ [decl]


header :: IParser Module.Header
header =
  do  option [] freshLine -- TODO: use comments
      (names, exports) <-
          option (["Main"], Var.openListing) (moduleDecl `followedBy` freshLine) -- TODO: use comments
      (docs, postDocsComments) <-
        choice
          [ (,) <$> addLocation (Just <$> docComment) <*> freshLine
          , (,) <$> addLocation (return Nothing) <*> return []
          ]
      imports' <- imports
      return (Module.Header names docs exports ((fmap Module.ImportComment postDocsComments) ++ imports'))


moduleDecl :: IParser ([String], Var.Listing (A.Located Var.Value))
moduleDecl =
  expecting "a module declaration" $
  do  try (reserved "module")
      whitespace -- TODO: use comments
      names <- dotSep1 capVar <?> "the name of this module"
      whitespace -- TODO: use comments
      exports <- option Var.openListing (listing (addLocation value))
      whitespace -- TODO: use comments
      reserved "where"
      return (names, exports)


imports :: IParser [Module.UserImport]
imports =
  concat <$> many ((:) <$> import' <*> (fmap Module.ImportComment <$> freshLine))


import' :: IParser Module.UserImport
import' =
  Module.UserImport <$> (
  expecting "an import" $
  addLocation $
  do  try (reserved "import")
      whitespace -- TODO: use comments
      names <- dotSep1 capVar
      (,) names <$> method (ModuleName.toString names)
  )
  where
    method :: String -> IParser Module.ImportMethod
    method originalName =
      Module.ImportMethod
          <$> option Nothing (Just <$> as' originalName)
          <*> option Var.closedListing exposing

    as' :: String -> IParser String
    as' moduleName =
      do  try (whitespace >> reserved "as") -- TODO: use comments
          whitespace -- TODO: use comments
          capVar <?> ("an alias for module `" ++ moduleName ++ "`")

    exposing :: IParser (Var.Listing Var.Value)
    exposing =
      do  try (whitespace >> reserved "exposing") -- TODO: use comments
          whitespace -- TODO: use comments
          listing value


listing :: IParser a -> IParser (Var.Listing a)
listing item =
  expecting "a listing of values and types to expose, like (..)" $
  do  try (whitespace >> char '(') -- TODO: use comments
      whitespace -- TODO: use comments
      listing <-
          choice
            [ const Var.openListing <$> string ".."
            , (\x -> Var.Listing $ x [] []) <$> commaSep1 (const . const <$> item) <*> return False -- TODO: use comments
            ]
      whitespace -- TODO: use comments
      char ')'
      return listing


value :: IParser Var.Value
value =
    val <|> tipe <?> "a value or type to expose"
  where
    val =
      Var.Value <$> ((Var.VarRef <$> lowVar) <|> parens' symOp)

    tipe =
      do  name <- capVar
          maybeCtors <- optionMaybe (listing capVar)
          case maybeCtors of
            Nothing -> return (Var.Alias name)
            Just ctors -> return (Var.Union name ctors)

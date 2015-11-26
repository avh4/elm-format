module Parse.Module (moduleDecl, header, getModuleName) where

import Text.Parsec hiding (newline, spaces)

import Parse.Helpers
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import Reporting.Annotation as A
import AST.V0_15
import qualified Util.List as List


getModuleName :: String -> Maybe String
getModuleName source =
  case iParse getModuleName source of
    Right name ->
        Just name

    Left _ ->
        Nothing
  where
    getModuleName =
      do  optional freshLine
          (names, _) <- moduleDecl
          return (ModuleName.toString names)


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

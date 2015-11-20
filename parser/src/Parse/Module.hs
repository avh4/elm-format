module Parse.Module (moduleDecl, header, getModuleName) where

import Text.Parsec hiding (newline, spaces)

import Parse.Helpers
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import Reporting.Annotation as A
import AST.V0_15


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
  do  optional freshLine
      (names, exports) <-
          option (["Main"], Var.openListing) (moduleDecl `followedBy` freshLine)
      docs <-
        choice
          [ addLocation (Just <$> docComment) `followedBy` freshLine
          , addLocation (return Nothing)
          ]
      imports' <- imports
      return (Module.Header names docs exports imports')


moduleDecl :: IParser ([String], Var.Listing (A.Located Var.Value))
moduleDecl =
  expecting "a module declaration" $
  do  try (reserved "module")
      whitespace
      names <- dotSep1 capVar <?> "the name of this module"
      whitespace
      exports <- option Var.openListing (listing (addLocation value))
      whitespace
      reserved "where"
      return (names, exports)


imports :: IParser [Module.UserImport]
imports =
  many (import' `followedBy` freshLine)


import' :: IParser Module.UserImport
import' =
  expecting "an import" $
  addLocation $
  do  try (reserved "import")
      whitespace
      names <- dotSep1 capVar
      (,) names <$> method (ModuleName.toString names)
  where
    method :: String -> IParser Module.ImportMethod
    method originalName =
      Module.ImportMethod
          <$> option Nothing (Just <$> as' originalName)
          <*> option Var.closedListing exposing

    as' :: String -> IParser String
    as' moduleName =
      do  try (whitespace >> reserved "as")
          whitespace
          capVar <?> ("an alias for module `" ++ moduleName ++ "`")

    exposing :: IParser (Var.Listing Var.Value)
    exposing =
      do  try (whitespace >> reserved "exposing")
          whitespace
          listing value


listing :: IParser a -> IParser (Var.Listing a)
listing item =
  expecting "a listing of values and types to expose, like (..)" $
  do  try (whitespace >> char '(')
      whitespace
      listing <-
          choice
            [ const Var.openListing <$> string ".."
            , (\x -> Var.Listing $ x [] []) <$> commaSep1 (const . const <$> item) <*> return False
            ]
      whitespace
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

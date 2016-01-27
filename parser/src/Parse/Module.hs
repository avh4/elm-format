module Parse.Module (moduleDecl, elmModule) where

import Text.Parsec hiding (newline, spaces)

import Parse.Helpers
import Parse.Declaration as Decl
import qualified AST.Declaration
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
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
      ((names, exports, postExports), preDocsComments) <-
          option
            ((Commented [] ["Main"] [], Var.openListing, []), [])
            ((,) <$> moduleDecl <*> freshLine)
      (docs, postDocsComments) <-
        choice
          [ (,) <$> addLocation (Just <$> docComment) <*> freshLine
          , (,) <$> addLocation (return Nothing) <*> return []
          ]
      imports' <- imports
      return (Module.Header names docs exports postExports ((fmap Module.ImportComment (preDocsComments ++ postDocsComments)) ++ imports'))


moduleDecl :: IParser (Commented ModuleName.Raw, Var.Listing Var.Value, Comments)
moduleDecl =
  expecting "a module declaration" $
  do  try (reserved "module")
      (_, preName) <- whitespace
      names <- dotSep1 capVar <?> "the name of this module"
      (_, postName) <- whitespace
      exports <- option Var.openListing (listing value)
      (_, preWhere) <- whitespace
      reserved "where"
      return (Commented preName names postName, exports, preWhere)


imports :: IParser [Module.UserImport]
imports =
  concat <$> many ((:) <$> import' <*> (fmap Module.ImportComment <$> freshLine))


import' :: IParser Module.UserImport
import' =
  Module.UserImport <$> (
  expecting "an import" $
  addLocation $
  do  try (reserved "import")
      (_, preName) <- whitespace
      names <- dotSep1 capVar
      method' <- method (ModuleName.toString names)
      return ((,) preName names, method')
  )
  where
    method :: String -> IParser Module.ImportMethod
    method originalName =
      Module.ImportMethod
        <$> option Nothing (Just <$> as' originalName)
        <*> option ([], ([], Var.closedListing)) exposing

    as' :: String -> IParser (Comments, PreCommented String)
    as' moduleName =
      do  (_, preAs) <- try (whitespace <* reserved "as")
          (_, postAs) <- whitespace
          (,) preAs <$> (,) postAs <$> capVar <?> ("an alias for module `" ++ moduleName ++ "`")

    exposing :: IParser (Comments, PreCommented (Var.Listing Var.Value))
    exposing =
      do  (_, preExposing) <- try (whitespace <* reserved "exposing")
          (_, postExposing) <- whitespace
          (,) preExposing <$> (,) postExposing <$> listing value


listing :: IParser a -> IParser (Var.Listing a)
listing item =
  expecting "a listing of values and types to expose, like (..)" $
  do  try (whitespace >> char '(') -- TODO: use comments
      (_, pre) <- whitespace
      listing <-
          choice
            [ const . const . const Var.openListing <$> string ".." -- TODO: use comments
            , (\x pre post -> Var.Listing (x pre post) False) <$> commaSep1' item
            ]
      (_, post) <- whitespace
      char ')'
      return $ listing pre post


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

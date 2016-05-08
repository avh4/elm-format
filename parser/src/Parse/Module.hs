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
  do  preModule <- option [] freshLine
      h <- moduleDecl
      preDocsComments <- option [] freshLine
      (docs, postDocsComments) <-
        choice
          [ (,) <$> addLocation (Just <$> docComment) <*> freshLine
          , (,) <$> addLocation (return Nothing) <*> return []
          ]
      imports' <- imports
      decls <- declarations
      trailingComments <-
          (++)
              <$> option [] freshLine
              <*> option [] spaces
      eof

      return $
        Module.Module
          preModule
          h
          docs
          ((fmap Module.ImportComment (preDocsComments ++ postDocsComments)) ++ imports')
          (decls ++ (map AST.Declaration.BodyComment trailingComments))


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


moduleDecl :: IParser Module.Header
moduleDecl =
  choice
    [ try moduleDecl_0_16
    , moduleDecl_0_17
    , return $
        Module.Header
          Module.UserModule
          (Commented [] ["Main"] [])
          []
          (Var.OpenListing (Commented [] () []))
    ]


moduleDecl_0_16 :: IParser Module.Header
moduleDecl_0_16 =
  expecting "a module declaration" $
  do  try (reserved "module")
      (_, preName) <- whitespace
      names <- dotSep1 capVar <?> "the name of this module"
      (_, postName1) <- whitespace
      (postName2, exports) <- option ([], Var.OpenListing (Commented [] () [])) (listing value)
      (_, preWhere) <- whitespace
      reserved "where"
      return $
        Module.Header
          Module.UserModule
          (Commented preName names (postName1 ++ postName2))
          preWhere
          exports


moduleDecl_0_17 :: IParser Module.Header
moduleDecl_0_17 =
  expecting "a module declaration" $
  do
      (moduleType, postPortComments) <-
        option
          (Module.UserModule, [])
          ((,) Module.PortModule <$> (reserved "port" *> (snd <$> whitespace)))
      try (reserved "module")
      (_, preName) <- whitespace
      names <- dotSep1 capVar <?> "the name of this module"
      (_, postName1) <- whitespace
      reserved "exposing"
      (preExports, exports) <- option ([], Var.OpenListing (Commented [] () [])) (listing value)
      return $
        Module.Header
          moduleType
          (Commented (postPortComments ++ preName) names postName1)
          preExports
          exports


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
        <*> option ([], ([], Var.ClosedListing)) exposing

    as' :: String -> IParser (Comments, PreCommented String)
    as' moduleName =
      do  (_, preAs) <- try (whitespace <* reserved "as")
          (_, postAs) <- whitespace
          (,) preAs <$> (,) postAs <$> capVar <?> ("an alias for module `" ++ moduleName ++ "`")

    exposing :: IParser (Comments, PreCommented (Var.Listing Var.Value))
    exposing =
      do  (_, preExposing) <- try (whitespace <* reserved "exposing")
          (_, postExposing) <- whitespace
          (postExposing2, listing') <- listing value
          return (preExposing, (postExposing ++ postExposing2, listing'))


listing :: IParser a -> IParser (Comments, Var.Listing a)
listing item =
  expecting "a listing of values and types to expose, like (..)" $
  do  (_, preParen) <- try (whitespace <* char '(')
      pushNewlineContext
      (_, pre) <- whitespace
      listing <-
          choice
            [ (\_ pre post _ -> (Var.OpenListing (Commented pre () post))) <$> string ".."
            , (\x pre post sawNewline ->
                (Var.ExplicitListing (x pre post) sawNewline))
                  <$> commaSep1' item
            ]
      (_, post) <- whitespace
      sawNewline <- popNewlineContext
      char ')'
      return $ (preParen, listing pre post sawNewline)


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
            Just (pre, ctors) -> return (Var.Union (name, pre) ctors)

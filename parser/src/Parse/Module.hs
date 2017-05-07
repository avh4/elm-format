module Parse.Module (moduleDecl, elmModule) where

import Text.Parsec hiding (newline, spaces)

import Parse.Helpers
import Parse.Declaration as Decl
import qualified AST.Declaration
import qualified AST.Module as Module
import qualified AST.Variable as Var
import AST.V0_16
import Parse.IParser
import Parse.Whitespace


elmModule :: IParser Module.Module
elmModule =
  do  preModule <- option [] freshLine
      h <- moduleDecl
      preDocsComments <- option [] freshLine
      (docs, postDocsComments) <-
        choice
          [ (,) <$> addLocation (Just <$> docCommentAsMarkdown) <*> freshLine
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
  (++) <$> ((\x -> [x]) <$> Decl.declaration)
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
          Module.Normal
          (Commented [] [UppercaseIdentifier "Main"] [])
          Nothing
          (KeywordCommented [] [] $ Var.OpenListing $ Commented [] () [])
    ]


moduleDecl_0_16 :: IParser Module.Header
moduleDecl_0_16 =
  expecting "a module declaration" $
  do  try (reserved "module")
      preName <- whitespace
      names <- dotSep1 capVar <?> "the name of this module"
      postName <- whitespace
      exports <- option (Var.OpenListing (Commented [] () [])) (listing value)
      preWhere <- whitespace
      reserved "where"
      return $
        Module.Header
          Module.Normal
          (Commented preName names postName)
          Nothing
          (KeywordCommented preWhere [] exports)


moduleDecl_0_17 :: IParser Module.Header
moduleDecl_0_17 =
  expecting "a module declaration" $
  do
      srcTag <-
        try $
            choice
                [ Module.Port <$> (reserved "port" *> whitespace)
                , Module.Effect <$> (reserved "effect" *> whitespace)
                , return Module.Normal
                ]
            <* reserved "module"
      preName <- whitespace
      names <- dotSep1 capVar <?> "the name of this module"
      whereClause <-
        optionMaybe $
          commentedKeyword "where" $
            brackets $ (\f pre post _ -> f pre post) <$> commaSep1 (keyValue equals lowVar capVar)

      exports <-
        commentedKeyword "exposing" $
          listing value

      return $
        Module.Header
          srcTag
          (Commented preName names [])
          whereClause
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
      preName <- whitespace
      names <- dotSep1 capVar
      method' <- method names
      return ((,) preName names, method')
  )
  where
    method :: [UppercaseIdentifier] -> IParser Module.ImportMethod
    method originalName =
      Module.ImportMethod
        <$> option Nothing (Just <$> as' originalName)
        <*> option ([], ([], Var.ClosedListing)) exposing

    as' :: [UppercaseIdentifier] -> IParser (Comments, PreCommented UppercaseIdentifier)
    as' moduleName =
      do  preAs <- try (whitespace <* reserved "as")
          postAs <- whitespace
          (,) preAs <$> (,) postAs <$> capVar <?> ("an alias for module `" ++ show moduleName ++ "`") -- TODO: do something correct instead of show

    exposing :: IParser (Comments, PreCommented (Var.Listing [Commented Var.Value]))
    exposing =
      do  preExposing <- try (whitespace <* reserved "exposing")
          postExposing <- whitespace
          imports <- listing value
          return (preExposing, (postExposing, imports))


listing :: IParser a -> IParser (Var.Listing [Commented a])
listing item =
  expecting "a listing of values and types to expose, like (..)" $
  do  _ <- try (char '(')
      pushNewlineContext
      pre <- whitespace
      listing <-
          choice
            [ (\_ pre post _ -> (Var.OpenListing (Commented pre () post))) <$> string ".."
            , (\x pre post sawNewline ->
                (Var.ExplicitListing (x pre post) sawNewline))
                  <$> commaSep1' item
            ]
      post <- whitespace
      sawNewline <- popNewlineContext
      char ')'
      return $ listing pre post sawNewline


listing' :: Ord a => IParser a -> IParser (Var.Listing (Var.CommentedMap a ()))
listing' item =
    expecting "a listing of values and types to expose, like (..)" $
    do  _ <- try (char '(')
        pushNewlineContext
        pre <- whitespace
        listing <-
            choice
              [ (\_ pre post _ -> (Var.OpenListing (Commented pre () post))) <$> string ".."
              , (\x pre post sawNewline ->
                  (Var.ExplicitListing (x pre post) sawNewline))
                    <$> commaSep1Set' ((\x -> (x, ())) <$> item) (\() () -> ())
              ]
        post <- whitespace
        sawNewline <- popNewlineContext
        char ')'
        return $ listing pre post sawNewline


value :: IParser Var.Value
value =
    val <|> tipe <?> "a value or type to expose"
  where
    val =
      (Var.Value <$> lowVar) <|> (Var.OpValue <$> parens' symOp)

    tipe =
      do  name <- capVar
          maybeCtors <- optionMaybe (try $ (,) <$> whitespace <*> listing' capVar)
          case maybeCtors of
            Nothing -> return $ Var.Union (name, []) Var.ClosedListing
            Just (pre, ctors) -> return (Var.Union (name, pre) ctors)

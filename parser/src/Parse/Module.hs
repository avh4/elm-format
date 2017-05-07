module Parse.Module (moduleDecl, elmModule) where

import Data.Map.Strict hiding (foldl, map)
import Elm.Utils ((|>))
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
      exports <- option (Var.OpenListing (Commented [] () [])) (listing detailedListing)
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
          listing detailedListing

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

    exposing :: IParser (Comments, PreCommented (Var.Listing Module.DetailedListing))
    exposing =
      do  preExposing <- try (whitespace <* reserved "exposing")
          postExposing <- whitespace
          imports <- listing detailedListing
          return (preExposing, (postExposing, imports))


listing :: IParser (Comments -> Comments -> a) -> IParser (Var.Listing a)
listing explicit =
  expecting "a listing of values and types to expose, like (..)" $
  do  _ <- try (char '(')
      pushNewlineContext
      pre <- whitespace
      listing <-
          choice
            [ (\_ pre post _ -> (Var.OpenListing (Commented pre () post))) <$> string ".."
            , (\x pre post sawNewline ->
                (Var.ExplicitListing (x pre post) sawNewline))
                  <$> explicit
            ]
      post <- whitespace
      sawNewline <- popNewlineContext
      char ')'
      return $ listing pre post sawNewline


commentedSet :: Ord a => IParser a -> IParser (Comments -> Comments -> Var.CommentedMap a ())
commentedSet item =
    commaSep1Set' ((\x -> (x, ())) <$> item) (\() () -> ())


detailedListing :: IParser (Comments -> Comments -> Module.DetailedListing)
detailedListing =
    do
      values <- commaSep1' value
      return $ \pre post -> toDetailedListing $ values pre post


mergeCommentedMap :: Ord k => (v -> v -> v) -> Var.CommentedMap k v -> Var.CommentedMap k v -> Var.CommentedMap k v
mergeCommentedMap merge left right =
    let
        merge' (Commented pre1 a post1) (Commented pre2 b post2) =
            Commented (pre1 ++ pre2) (merge a b) (post1 ++ post2)
    in
    unionWith merge' left right


mergeListing :: (a -> a -> a) -> Var.Listing a -> Var.Listing a -> Var.Listing a
mergeListing merge left right =
    case (left, right) of
        (Var.ClosedListing, Var.ClosedListing) -> Var.ClosedListing
        (Var.ClosedListing, Var.OpenListing comments) -> Var.OpenListing comments
        (Var.OpenListing comments, Var.ClosedListing) -> Var.OpenListing comments
        (Var.OpenListing (Commented pre1 () post1), Var.OpenListing (Commented pre2 () post2)) -> Var.OpenListing (Commented (pre1 ++ pre2) () (post1 ++ post2))
        (Var.ClosedListing, Var.ExplicitListing a multiline) -> Var.ExplicitListing a multiline
        (Var.ExplicitListing a multiline, Var.ClosedListing) -> Var.ExplicitListing a multiline
        (Var.OpenListing comments, Var.ExplicitListing a multiline) -> Var.ExplicitListing a multiline -- NOTE: we drop the open listing comments
        (Var.ExplicitListing a multiline, Var.OpenListing comments) -> Var.ExplicitListing a multiline -- NOTE: we drop the open listing comments
        (Var.ExplicitListing a multiline1, Var.ExplicitListing b multiline2) -> Var.ExplicitListing (merge a b) (multiline1 || multiline2)


toDetailedListing :: [Commented Var.Value] -> Module.DetailedListing
toDetailedListing values =
    let
        merge
            (Commented pre1 (inner1, tags1) post1)
            (Commented pre2 (inner2, tags2) post2)
            =
            Commented
                (pre1 ++ pre2)
                ( inner1 ++ inner2
                , mergeListing (mergeCommentedMap (\() () -> ())) tags1 tags2
                )
                (post1 ++ post2)


        step (vs, os, ts) (Commented pre val post) =
            case val of
                Var.Value name ->
                    (insert name (Commented pre () post) vs, os, ts)
                Var.OpValue name ->
                    (vs, insert name (Commented pre () post) os, ts)
                Var.Union (name, inner) tags ->
                    (vs, os, insertWith merge name (Commented pre (inner, tags) post) ts)

        done (vs, os, ts) =
            Module.DetailedListing vs os ts
    in
    foldl step (empty, empty, empty) values
        |> done


value :: IParser Var.Value
value =
    val <|> tipe <?> "a value or type to expose"
  where
    val =
      (Var.Value <$> lowVar) <|> (Var.OpValue <$> parens' symOp)

    tipe =
      do  name <- capVar
          maybeCtors <- optionMaybe (try $ (,) <$> whitespace <*> listing (commentedSet capVar))
          case maybeCtors of
            Nothing -> return $ Var.Union (name, []) Var.ClosedListing
            Just (pre, ctors) -> return (Var.Union (name, pre) ctors)

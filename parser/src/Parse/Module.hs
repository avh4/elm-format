module Parse.Module (moduleDecl, elmModule, topLevel) where

import qualified Control.Applicative
import Data.Map.Strict hiding (foldl, map)
import Elm.Utils ((|>))
import Text.Parsec hiding (newline, spaces)

import Parse.Helpers
import Parse.Declaration as Decl
import qualified AST.Declaration
import qualified AST.Module as Module
import qualified AST.Variable as Var
import AST.V0_16
import ElmVersion
import Parse.IParser
import Parse.Whitespace


elmModule :: ElmVersion -> IParser Module.Module
elmModule elmVersion =
  do  preModule <- option [] freshLine
      h <- moduleDecl elmVersion
      preDocsComments <- option [] freshLine
      (docs, postDocsComments) <-
        choice
          [ (,) <$> addLocation (Just <$> docCommentAsMarkdown) <*> freshLine
          , (,) <$> addLocation (return Nothing) <*> return []
          ]
      (preImportComments, imports', postImportComments) <- imports elmVersion
      decls <- topLevel $ Decl.declaration elmVersion
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
          (preDocsComments ++ postDocsComments ++ preImportComments, imports')
          ((map AST.Declaration.BodyComment postImportComments) ++ decls ++ (map AST.Declaration.BodyComment trailingComments))


topLevel :: IParser a -> IParser [AST.Declaration.TopLevelStructure a]
topLevel entry =
  (++) <$> option [] (((\x -> [x]) <$> Decl.topLevelStructure entry))
      <*> (concat <$> many (freshDef entry))


freshDef :: IParser a -> IParser [AST.Declaration.TopLevelStructure a]
freshDef entry =
    commitIf (freshLine >> (letter <|> char '_')) $
      do  comments <- freshLine
          decl <- Decl.topLevelStructure entry
          return $ (map AST.Declaration.BodyComment comments) ++ [decl]


moduleDecl :: ElmVersion -> IParser (Maybe Module.Header)
moduleDecl elmVersion =
  choice
    [ try $ Just <$> moduleDecl_0_16 elmVersion
    , Just <$> moduleDecl_0_17 elmVersion
    , return Nothing
    ]


moduleDecl_0_16 :: ElmVersion -> IParser Module.Header
moduleDecl_0_16 elmVersion =
  expecting "a module declaration" $
  do  try (reserved elmVersion "module")
      preName <- whitespace
      names <- dotSep1 (capVar elmVersion) <?> "the name of this module"
      postName <- whitespace
      exports <- option (Var.OpenListing (Commented [] () [])) (listing $ detailedListing elmVersion)
      preWhere <- whitespace
      reserved elmVersion "where"
      return $
        Module.Header
          Module.Normal
          (Commented preName names postName)
          Nothing
          (Just $ KeywordCommented preWhere [] exports)


moduleDecl_0_17 :: ElmVersion -> IParser Module.Header
moduleDecl_0_17 elmVersion =
  expecting "a module declaration" $
  do
      srcTag <-
        try $
            choice
                [ Module.Port <$> (reserved elmVersion "port" *> whitespace)
                , Module.Effect <$> (reserved elmVersion "effect" *> whitespace)
                , return Module.Normal
                ]
            <* reserved elmVersion "module"
      preName <- whitespace
      names <- dotSep1 (capVar elmVersion) <?> "the name of this module"
      whereClause <-
        optionMaybe $
          commentedKeyword elmVersion "where" $
            brackets $ (\f pre post _ -> f pre post) <$> commaSep1 (keyValue equals (lowVar elmVersion) (capVar elmVersion))

      exports <-
        optionMaybe $
        commentedKeyword elmVersion "exposing" $
          listing $ detailedListing elmVersion

      return $
        Module.Header
          srcTag
          (Commented preName names [])
          whereClause
          exports


mergePreCommented :: (a -> a -> a) -> PreCommented a -> PreCommented a -> PreCommented a
mergePreCommented merge (pre1, left) (pre2, right) =
    (pre1 ++ pre2, merge left right)


mergeDetailedListing :: Module.DetailedListing -> Module.DetailedListing -> Module.DetailedListing
mergeDetailedListing left right =
    Module.DetailedListing
        (mergeCommentedMap (\() () -> ()) (Module.values left) (Module.values right))
        (mergeCommentedMap (\() () -> ()) (Module.operators left) (Module.operators right))
        (mergeCommentedMap (mergePreCommented $ mergeListing $ mergeCommentedMap (\() () -> ())) (Module.types left) (Module.types right))


imports :: ElmVersion -> IParser (Comments, Map [UppercaseIdentifier] (Comments, Module.ImportMethod), Comments)
imports elmVersion =
    let
        merge :: PreCommented Module.ImportMethod -> PreCommented Module.ImportMethod -> PreCommented Module.ImportMethod
        merge (comments1, import1) (comments2, import2) =
            ( comments1 ++ comments2
            , Module.ImportMethod
                (Module.alias import1 Control.Applicative.<|> Module.alias import2)
                (mergePreCommented (mergePreCommented $ mergeListing mergeDetailedListing) (Module.exposedVars import1) (Module.exposedVars import2))
            )

        step (comments, m, finalComments) (((pre, name), method), post) =
            ( comments ++ finalComments
            , insertWith merge name (pre, method) m
            , post
            )

        done :: [(Module.UserImport, Comments)] -> (Comments, Map [UppercaseIdentifier] (Comments, Module.ImportMethod), Comments)
        done results =
            foldl step ([], empty, []) results
    in
    done <$> many ((,) <$> import' elmVersion <*> freshLine)


import' :: ElmVersion -> IParser Module.UserImport
import' elmVersion =
  expecting "an import" $
  do  try (reserved elmVersion "import")
      preName <- whitespace
      names <- dotSep1 $ capVar elmVersion
      method' <- method names
      return ((,) preName names, method')
  where
    method :: [UppercaseIdentifier] -> IParser Module.ImportMethod
    method originalName =
      Module.ImportMethod
        <$> option Nothing (Just <$> as' originalName)
        <*> option ([], ([], Var.ClosedListing)) exposing

    as' :: [UppercaseIdentifier] -> IParser (Comments, PreCommented UppercaseIdentifier)
    as' moduleName =
      do  preAs <- try (whitespace <* reserved elmVersion "as")
          postAs <- whitespace
          (,) preAs <$> (,) postAs <$> capVar elmVersion <?> ("an alias for module `" ++ show moduleName ++ "`") -- TODO: do something correct instead of show

    exposing :: IParser (Comments, PreCommented (Var.Listing Module.DetailedListing))
    exposing =
      do  preExposing <- try (whitespace <* reserved elmVersion "exposing")
          postExposing <- whitespace
          imports <-
            choice
              [ listing $ detailedListing elmVersion
              , listingWithoutParens elmVersion
              ]
          return (preExposing, (postExposing, imports))


listing :: IParser (Comments -> Comments -> a) -> IParser (Var.Listing a)
listing explicit =
  let
    subparser = choice
        [ (\_ pre post _ -> (Var.OpenListing (Commented pre () post))) <$> string ".."
        , (\x pre post sawNewline -> (Var.ExplicitListing (x pre post) sawNewline)) <$>
            explicit
        ]
  in
    expecting "a listing of values and types to expose, like (..)" $
    do  _ <- try (char '(')
        ((pre, listing, post), multiline) <- trackNewline ((,,) <$> whitespace <*> subparser <*> whitespace)
        _ <- char ')'
        return $ listing pre post $ multilineToBool multiline


listingWithoutParens :: ElmVersion -> IParser (Var.Listing Module.DetailedListing)
listingWithoutParens elmVersion =
  expecting "a listing of values and types to expose, but with missing parentheses" $
  choice
    [ (\_ -> (Var.OpenListing (Commented [] () []))) <$> string ".."
    , (\x -> (Var.ExplicitListing (x [] []) False)) <$> detailedListing elmVersion
    ]


commentedSet :: Ord a => IParser a -> IParser (Comments -> Comments -> Var.CommentedMap a ())
commentedSet item =
    commaSep1Set' ((\x -> (x, ())) <$> item) (\() () -> ())


detailedListing :: ElmVersion -> IParser (Comments -> Comments -> Module.DetailedListing)
detailedListing elmVersion =
    do
      values <- commaSep1' (value elmVersion)
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
        (Var.OpenListing comments, Var.ExplicitListing _a _multiline) -> Var.OpenListing comments
        (Var.ExplicitListing _a _multiline, Var.OpenListing comments) -> Var.OpenListing comments
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


value :: ElmVersion -> IParser Var.Value
value elmVersion =
    val <|> tipe <?> "a value or type to expose"
  where
    val =
      (Var.Value <$> lowVar elmVersion) <|> (Var.OpValue <$> parens' symOp)

    tipe =
      do  name <- capVar elmVersion
          maybeCtors <- optionMaybe (try $ (,) <$> whitespace <*> listing (commentedSet $ capVar elmVersion))
          case maybeCtors of
            Nothing -> return $ Var.Union (name, []) Var.ClosedListing
            Just (pre, ctors) -> return (Var.Union (name, pre) ctors)

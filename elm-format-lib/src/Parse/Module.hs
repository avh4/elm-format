{-# LANGUAGE DataKinds #-}
module Parse.Module (moduleDecl, elmModule, topLevel, import') where

import qualified Control.Applicative
import Data.Map.Strict ( Map, empty, insert, insertWith )
import Elm.Utils ((|>))
import Parse.ParsecAdapter ( char, letter, string, choice, eof, option, optionMaybe, (<?>), (<|>), many, try )
import Parse.Helpers
import qualified Parse.Declaration as Decl
import AST.V0_16 hiding (imports)
import qualified Data.Indexed as I
import ElmVersion
import Parse.IParser
import Parse.Whitespace


elmModule :: ElmVersion -> IParser (ParsedAST 'ModuleNK)
elmModule elmVersion =
  fmap I.Fix2 $ addLocation $
  do  preModule <- option [] freshLine
      h <- moduleDecl elmVersion
      preDocsComments <- option [] freshLine
      (docs, postDocsComments) <-
        choice
          [ (,) <$> (Just <$> docCommentAsMarkdown) <*> freshLine
          , return (Nothing, [])
          ]
      (preImportComments, imports', postImportComments) <- imports elmVersion
      topLevels <-
          do
              decls <- topLevel $ Decl.declaration elmVersion
              trailingComments <-
                  (++)
                      <$> option [] freshLine
                      <*> option [] spaces
              eof
              return (fmap BodyComment postImportComments ++ decls ++ fmap BodyComment trailingComments)

      return $
        Module
          preModule
          h
          docs
          (C (preDocsComments ++ postDocsComments ++ preImportComments) imports')
          topLevels


topLevel :: IParser a -> IParser [TopLevelStructure a]
topLevel entry =
  (++) <$> option [] (pure <$> Decl.topLevelStructure entry)
      <*> (concat <$> many (freshDef entry))


freshDef :: IParser a -> IParser [TopLevelStructure a]
freshDef entry =
    commitIf (freshLine >> (letter <|> char '_')) $
      do  comments <- freshLine
          decl <- Decl.topLevelStructure entry
          return $ fmap BodyComment comments ++ [decl]


moduleDecl :: ElmVersion -> IParser (Maybe (ParsedAST 'ModuleHeaderNK))
moduleDecl elmVersion =
  choice
    [ try $ Just <$> moduleDecl_0_16 elmVersion
    , Just <$> moduleDecl_0_17 elmVersion
    , return Nothing
    ]


moduleDecl_0_16 :: ElmVersion -> IParser (ParsedAST 'ModuleHeaderNK)
moduleDecl_0_16 elmVersion =
  expecting "a module declaration" $
  fmap I.Fix2 $ addLocation $
  do  try (reserved elmVersion "module")
      preName <- whitespace
      names <- dotSep1 (capVar elmVersion) <?> "the name of this module"
      postName <- whitespace
      exports <- option (OpenListing (C ([], []) ())) (listing $ detailedListing elmVersion)
      preWhere <- whitespace
      reserved elmVersion "where"
      return $
        ModuleHeader
          Normal
          (C (preName, postName) names)
          Nothing
          (Just $ C (preWhere, []) exports)


moduleDecl_0_17 :: ElmVersion -> IParser (ParsedAST 'ModuleHeaderNK)
moduleDecl_0_17 elmVersion =
  expecting "a module declaration" $
  fmap I.Fix2 $ addLocation $
  do
      srcTag <-
        try $
            choice
                [ Port <$> (reserved elmVersion "port" *> whitespace)
                , Effect <$> (reserved elmVersion "effect" *> whitespace)
                , return Normal
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
        commentedKeyword elmVersion "exposing" (listing $ detailedListing elmVersion)
          <|> try (listingWithoutExposing elmVersion)

      return $
        ModuleHeader
          srcTag
          (C (preName, []) names)
          whereClause
          exports

listingWithoutExposing :: ElmVersion -> IParser (C2 beforeKeyword afterKeyword (Listing DetailedListing))
listingWithoutExposing elmVersion = do
    let pre = []
    post <- whitespace
    C (pre, post) <$> listing (detailedListing elmVersion)

mergePreCommented :: (a -> a -> a) -> C1 before a -> C1 before a -> C1 before a
mergePreCommented merge (C pre1 left) (C pre2 right) =
    C (pre1 ++ pre2) (merge left right)

mergeC2 :: (a -> b -> c) -> C2 before after a -> C2 before after b -> C2 before after c
mergeC2 merge (C (pre1, post1) left) (C (pre2, post2) right) =
    C (pre1 ++ pre2, post1 ++ post2) (merge left right)


mergeDetailedListing :: DetailedListing -> DetailedListing -> DetailedListing
mergeDetailedListing left right =
    DetailedListing
        (mergeCommentedMap (<>) (values left) (values right))
        (mergeCommentedMap (<>) (operators left) (operators right))
        (mergeCommentedMap (mergePreCommented $ mergeListing $ mergeCommentedMap (<>)) (types left) (types right))


imports :: ElmVersion -> IParser (Comments, Map [UppercaseIdentifier] (C1 'BeforeTerm ImportMethod), Comments)
imports elmVersion =
    let
        merge :: C1 'BeforeTerm ImportMethod -> C1 'BeforeTerm ImportMethod -> C1 'BeforeTerm ImportMethod
        merge (C comments1 import1) (C comments2 import2) =
            C (comments1 ++ comments2) $
                ImportMethod
                    (alias import1 Control.Applicative.<|> alias import2)
                    (mergeC2 (mergeListing mergeDetailedListing) (exposedVars import1) (exposedVars import2))

        step (comments, m, finalComments) (((C pre name), method), post) =
            ( comments ++ finalComments
            , insertWith merge name (C pre method) m
            , post
            )

        done :: [(UserImport, Comments)] -> (Comments, Map [UppercaseIdentifier] (C1 'BeforeTerm ImportMethod), Comments)
        done results =
            foldl step ([], empty, []) results
    in
    done <$> many ((,) <$> import' elmVersion <*> freshLine)


import' :: ElmVersion -> IParser UserImport
import' elmVersion =
  expecting "an import" $
  do  try (reserved elmVersion "import")
      preName <- whitespace
      names <- dotSep1 $ capVar elmVersion
      method' <- method names
      return (C preName names, method')
  where
    method :: [UppercaseIdentifier] -> IParser ImportMethod
    method originalName =
      ImportMethod
        <$> option Nothing (Just <$> as' originalName)
        <*> option (C ([], []) ClosedListing) (exposing <|> try (listingWithoutExposing elmVersion))

    as' :: [UppercaseIdentifier] -> IParser (C2 'BeforeSeparator 'AfterSeparator UppercaseIdentifier)
    as' moduleName =
      do  preAs <- try (whitespace <* reserved elmVersion "as")
          postAs <- whitespace
          C (preAs, postAs) <$> capVar elmVersion <?> ("an alias for module `" ++ show moduleName ++ "`") -- TODO: do something correct instead of show

    exposing :: IParser (C2 'BeforeSeparator 'AfterSeparator (Listing DetailedListing))
    exposing =
      do  preExposing <- try (whitespace <* reserved elmVersion "exposing")
          postExposing <- whitespace
          imports <-
            choice
              [ listing $ detailedListing elmVersion
              , listingWithoutParens elmVersion
              ]
          return $ C (preExposing, postExposing) imports


listing :: IParser (Comments -> Comments -> a) -> IParser (Listing a)
listing explicit =
  let
    subparser = choice
        [ (\_ pre post _ -> (OpenListing (C (pre, post) ()))) <$> string ".."
        , (\x pre post sawNewline -> (ExplicitListing (x pre post) sawNewline)) <$>
            explicit
        ]
  in
    expecting "a listing of values and types to expose, like (..)" $
    do  _ <- try (char '(')
        ((pre, listing, post), multiline) <- trackNewline ((,,) <$> whitespace <*> subparser <*> whitespace)
        _ <- char ')'
        return $ listing pre post $ multilineToBool multiline


listingWithoutParens :: ElmVersion -> IParser (Listing DetailedListing)
listingWithoutParens elmVersion =
  expecting "a listing of values and types to expose, but with missing parentheses" $
  choice
    [ (\_ -> (OpenListing (C ([], []) ()))) <$> string ".."
    , (\x -> (ExplicitListing (x [] []) False)) <$> detailedListing elmVersion
    ]


commentedSet :: Ord a => IParser a -> IParser (Comments -> Comments -> CommentedMap a ())
commentedSet item =
    commaSep1Set' ((\x -> (x, ())) <$> item) (\() () -> ())


detailedListing :: ElmVersion -> IParser (Comments -> Comments -> DetailedListing)
detailedListing elmVersion =
    do
      values <- commaSep1' (value elmVersion)
      return $ \pre post -> toDetailedListing $ values pre post


toDetailedListing :: [C2 before after ListingValue] -> DetailedListing
toDetailedListing values =
    let
        merge
            (C (pre1, post1) (C inner1 tags1))
            (C (pre2, post2) (C inner2 tags2))
            =
            C (pre1 ++ pre2, post1 ++ post2) $
                C (inner1 ++ inner2) $
                    mergeListing (mergeCommentedMap (<>)) tags1 tags2

        step (vs, os, ts) (C (pre, post) val) =
            case val of
                Value name ->
                    (insert name (C (pre, post) ()) vs, os, ts)
                OpValue name ->
                    (vs, insert name (C (pre, post) ()) os, ts)
                Union (C inner name) tags ->
                    (vs, os, insertWith merge name (C (pre, post) (C inner tags)) ts)

        done (vs, os, ts) =
            DetailedListing vs os ts
    in
    foldl step (empty, empty, empty) values
        |> done


value :: ElmVersion -> IParser ListingValue
value elmVersion =
    val <|> tipe <?> "a value or type to expose"
  where
    val =
      (Value <$> lowVar elmVersion) <|> (OpValue <$> parens' symOp)

    tipe =
      do  name <- capVar elmVersion
          maybeCtors <- optionMaybe (try $ (,) <$> whitespace <*> listing (commentedSet $ capVar elmVersion))
          case maybeCtors of
            Nothing -> return $ Union (C [] name) ClosedListing
            Just (pre, ctors) -> return (Union (C pre name) ctors)

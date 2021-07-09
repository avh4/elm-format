{-# LANGUAGE DataKinds #-}
module Parse.Module (moduleDecl, elmModule, topLevel, import') where

import qualified Control.Applicative
import Data.Map.Strict ( Map, empty, insert, insertWith )
import Elm.Utils ((|>))
import Parse.ParsecAdapter ( char, letter, string, choice, eof, option, optionMaybe, (<?>), (<|>), many, try )
import Parse.Helpers
import qualified Parse.Declaration as Decl
import AST.Listing (Listing(..), mergeCommentedMap, mergeListing)
import qualified AST.Listing as Listing
import AST.Module (DetailedListing, Module, ImportMethod)
import qualified AST.Module as Module
import AST.Structure
import AST.V0_16
import qualified Data.Indexed as I
import ElmVersion
import Parse.IParser
import Parse.Whitespace
import Reporting.Annotation (Located)


elmModule :: ElmVersion -> IParser (Module [UppercaseIdentifier] (ASTNS Located [UppercaseIdentifier] 'TopLevelNK))
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
      topLevels <-
          fmap I.Fix $
          addLocation $
          fmap TopLevel $
          do
              decls <- topLevel $ Decl.declaration elmVersion
              trailingComments <-
                  (++)
                      <$> option [] freshLine
                      <*> option [] spaces
              eof
              return ((map BodyComment postImportComments) ++ decls ++ (map BodyComment trailingComments))

      return $
        Module.Module
          preModule
          h
          docs
          (C (preDocsComments ++ postDocsComments ++ preImportComments) imports')
          topLevels


topLevel :: IParser a -> IParser [TopLevelStructure a]
topLevel entry =
  (++) <$> option [] (((\x -> [x]) <$> Decl.topLevelStructure entry))
      <*> (concat <$> many (freshDef entry))


freshDef :: IParser a -> IParser [TopLevelStructure a]
freshDef entry =
    commitIf (freshLine >> (letter <|> char '_')) $
      do  comments <- freshLine
          decl <- Decl.topLevelStructure entry
          return $ (map BodyComment comments) ++ [decl]


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
      exports <- option (OpenListing (C ([], []) ())) (listing $ detailedListing elmVersion)
      preWhere <- whitespace
      reserved elmVersion "where"
      return $
        Module.Header
          Module.Normal
          (C (preName, postName) names)
          Nothing
          (Just $ C (preWhere, []) exports)


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
        commentedKeyword elmVersion "exposing" (listing $ detailedListing elmVersion)
          <|> try (listingWithoutExposing elmVersion)

      return $
        Module.Header
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


mergeDetailedListing :: Module.DetailedListing -> Module.DetailedListing -> Module.DetailedListing
mergeDetailedListing left right =
    Module.DetailedListing
        (mergeCommentedMap (<>) (Module.values left) (Module.values right))
        (mergeCommentedMap (<>) (Module.operators left) (Module.operators right))
        (mergeCommentedMap (mergePreCommented $ mergeListing $ mergeCommentedMap (<>)) (Module.types left) (Module.types right))


imports :: ElmVersion -> IParser (Comments, Map [UppercaseIdentifier] (C1 'BeforeTerm ImportMethod), Comments)
imports elmVersion =
    let
        merge :: C1 'BeforeTerm ImportMethod -> C1 'BeforeTerm ImportMethod -> C1 'BeforeTerm ImportMethod
        merge (C comments1 import1) (C comments2 import2) =
            C (comments1 ++ comments2) $
                Module.ImportMethod
                    (Module.alias import1 Control.Applicative.<|> Module.alias import2)
                    (mergeC2 (mergeListing mergeDetailedListing) (Module.exposedVars import1) (Module.exposedVars import2))

        step (comments, m, finalComments) (((C pre name), method), post) =
            ( comments ++ finalComments
            , insertWith merge name (C pre method) m
            , post
            )

        done :: [(Module.UserImport, Comments)] -> (Comments, Map [UppercaseIdentifier] (C1 'BeforeTerm ImportMethod), Comments)
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
      return (C preName names, method')
  where
    method :: [UppercaseIdentifier] -> IParser Module.ImportMethod
    method originalName =
      Module.ImportMethod
        <$> option Nothing (Just <$> as' originalName)
        <*> option (C ([], []) ClosedListing) (exposing <|> try (listingWithoutExposing elmVersion))

    as' :: [UppercaseIdentifier] -> IParser (C2 'BeforeSeparator 'AfterSeparator UppercaseIdentifier)
    as' moduleName =
      do  preAs <- try (whitespace <* reserved elmVersion "as")
          postAs <- whitespace
          C (preAs, postAs) <$> capVar elmVersion <?> ("an alias for module `" ++ show moduleName ++ "`") -- TODO: do something correct instead of show

    exposing :: IParser (C2 'BeforeSeparator 'AfterSeparator (Listing Module.DetailedListing))
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


listingWithoutParens :: ElmVersion -> IParser (Listing Module.DetailedListing)
listingWithoutParens elmVersion =
  expecting "a listing of values and types to expose, but with missing parentheses" $
  choice
    [ (\_ -> (OpenListing (C ([], []) ()))) <$> string ".."
    , (\x -> (ExplicitListing (x [] []) False)) <$> detailedListing elmVersion
    ]


commentedSet :: Ord a => IParser a -> IParser (Comments -> Comments -> Listing.CommentedMap a ())
commentedSet item =
    commaSep1Set' ((\x -> (x, ())) <$> item) (\() () -> ())


detailedListing :: ElmVersion -> IParser (Comments -> Comments -> Module.DetailedListing)
detailedListing elmVersion =
    do
      values <- commaSep1' (value elmVersion)
      return $ \pre post -> toDetailedListing $ values pre post


toDetailedListing :: [C2 before after Listing.Value] -> Module.DetailedListing
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
                Listing.Value name ->
                    (insert name (C (pre, post) ()) vs, os, ts)
                Listing.OpValue name ->
                    (vs, insert name (C (pre, post) ()) os, ts)
                Listing.Union (C inner name) tags ->
                    (vs, os, insertWith merge name (C (pre, post) (C inner tags)) ts)

        done (vs, os, ts) =
            Module.DetailedListing vs os ts
    in
    foldl step (empty, empty, empty) values
        |> done


value :: ElmVersion -> IParser Listing.Value
value elmVersion =
    val <|> tipe <?> "a value or type to expose"
  where
    val =
      (Listing.Value <$> lowVar elmVersion) <|> (Listing.OpValue <$> parens' symOp)

    tipe =
      do  name <- capVar elmVersion
          maybeCtors <- optionMaybe (try $ (,) <$> whitespace <*> listing (commentedSet $ capVar elmVersion))
          case maybeCtors of
            Nothing -> return $ Listing.Union (C [] name) Listing.ClosedListing
            Just (pre, ctors) -> return (Listing.Union (C pre name) ctors)

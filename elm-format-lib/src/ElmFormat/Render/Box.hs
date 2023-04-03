{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use list comprehension" #-}

module ElmFormat.Render.Box where

import Elm.Utils ((|>))
import Box hiding (rowOrStackForce, Line, Box, mustBreak)
import ElmVersion (ElmVersion(..))

import AST.V0_16
import qualified AST.Module
import AST.Structure
import qualified AST.Listing
import qualified Cheapskate.Types as Markdown
import qualified Data.Char as Char
import Data.Coapplicative
import qualified Data.Foldable as Foldable
import Data.Functor.Identity
import qualified Data.Indexed as I
import qualified Data.List as List
import Data.List.Extra
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList, listToMaybe, catMaybes)
import qualified Data.Maybe as Maybe
import Data.ReversedList (Reversed)
import qualified Data.ReversedList as ReversedList
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import ElmFormat.ImportInfo (ImportInfo)
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified ElmFormat.Render.ElmStructure as ElmStructure
import qualified ElmFormat.Render.Markdown
import qualified ElmVersion
import qualified Parse.Parse as Parse
import qualified Reporting.Annotation as A
import qualified Reporting.Result as Result
import Text.Printf (printf)
import qualified Box.BlockAdapter as Block
import Box.BlockAdapter (Line, Block)
import qualified Data.Bifunctor as Bifunctor
import ElmFormat.Render.ElmStructure
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Semigroup(sconcat))
import AST.Listing (Listing(..))

pleaseReport'' :: String -> String -> String
pleaseReport'' what details =
    -- TODO: include version in the message
    "<elm-format: "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >"


pleaseReport' :: String -> String -> Line
pleaseReport' what details =
    keyword $ pleaseReport'' what details


pleaseReport :: String -> String -> Block
pleaseReport what details =
    line $ pleaseReport' what details


surround :: Char -> Char -> Block -> Block
surround open close block =
  Block.rowOrStack
    Nothing
    [ Block.prefix 1 (Block.char7 open) block,
      Block.line $ Block.char7 close
    ]


parens :: Block -> Block
parens = surround '(' ')'


spaceSep :: NonEmpty Line -> Line
spaceSep = sconcat . NonEmpty.intersperse space


formatBinary :: Bool -> Block -> [ ( Bool, Comments, String, Block ) ] -> Block
formatBinary multiline left ops =
    case ops of
        [] ->
            left

        ( isLeftPipe, comments, op, next ) : rest ->
            if isLeftPipe then
                spaceSepOrIndentedForce multiline
                    [ spaceSepOrStack $ left :|
                        concat @[]
                            [ Maybe.maybeToList $ formatComments comments
                            , [line $ Block.stringUtf8 op]
                            ]
                    , formatBinary multiline next rest
                    ]
            else
                formatBinary
                    multiline
                    (spaceSepOrIndentedForce multiline
                        [ left
                        , formatCommented' comments $
                            spaceSepOrPrefix
                                (fromIntegral $ length op)
                                (Block.stringUtf8 op) next
                        ])
                    rest


splitWhere :: (a -> Bool) -> [a] -> [[a]]
splitWhere predicate list =
    let
        merge acc result =
            ReversedList.push (ReversedList.toList acc) result

        step (acc,result) next =
            if predicate next then
                (ReversedList.empty, merge (ReversedList.push next acc) result)
            else
                (ReversedList.push next acc, result)
    in
      list
          |> foldl step (ReversedList.empty, ReversedList.empty)
          |> uncurry merge
          |> ReversedList.toList
          |> dropWhile null


data DeclarationType
  = DComment
  | DStarter
  | DCloser
  | DDefinition (Maybe (Ref ()))
  | DFixity
  | DDocComment
  deriving (Show)


declarationType :: TopLevelStructure BodyEntryType -> DeclarationType
declarationType decl =
  case decl of
    Entry entry ->
        case entry of
            BodyNamed name -> DDefinition (Just name)
            BodyUnnamed -> DDefinition Nothing
            BodyFixity -> DFixity

    DocComment _ ->
      DDocComment

    BodyComment CommentTrickOpener ->
      DStarter

    BodyComment CommentTrickCloser ->
      DCloser

    BodyComment _ ->
      DComment


removeDuplicates :: Ord a => [[a]] -> [[a]]
removeDuplicates input =
    foldl step (ReversedList.empty, Set.empty) input |> fst |> ReversedList.toList
    where
        step :: Ord a => (Reversed [a], Set a) -> [a] -> (Reversed [a], Set a)
        step (acc, seen) next =
            case foldl stepChildren (ReversedList.empty, seen) next |> Bifunctor.first ReversedList.toList of
                ([], seen') -> (acc, seen')
                (children', seen') -> (ReversedList.push children' acc, seen')

        stepChildren :: Ord a => (Reversed a, Set a) -> a -> (Reversed a, Set a)
        stepChildren (acc, seen) next =
            if Set.member next seen
                then (acc, seen)
                else (ReversedList.push next acc, Set.insert next seen)


sortVars :: Bool -> Set (C2 before after AST.Listing.Value) -> [[String]] -> ([[C2 before after AST.Listing.Value]], Comments)
sortVars forceMultiline fromExposing fromDocs =
    let
        varOrder :: Commented c AST.Listing.Value -> (Int, String)
        varOrder (C _ (AST.Listing.OpValue (SymbolIdentifier name))) = (1, name)
        varOrder (C _ (AST.Listing.Union (C _ (UppercaseIdentifier name)) _)) = (2, name)
        varOrder (C _ (AST.Listing.Value (LowercaseIdentifier name))) = (3, name)

        listedInDocs =
            fromDocs
                |> fmap (Maybe.mapMaybe (\v -> Map.lookup v allowedInDocs))
                |> filter (not . List.null)
                |> fmap (fmap (C ([], [])))
                |> removeDuplicates

        listedInExposing =
            fromExposing
                |> Set.toList
                |> List.sortOn varOrder

        varName (C _ (AST.Listing.Value (LowercaseIdentifier name))) = name
        varName (C _ (AST.Listing.OpValue (SymbolIdentifier name))) = name
        varName (C _ (AST.Listing.Union (C _ (UppercaseIdentifier name)) _)) = name

        varSetToMap set =
            Set.toList set
                |> fmap (\(C c var)-> (varName (C c var), var))
                |> Map.fromList

        allowedInDocs =
            varSetToMap fromExposing

        allFromDocs =
            Set.fromList $ fmap varName $ concat listedInDocs

        inDocs x =
            Set.member (varName x) allFromDocs

        remainingFromExposing =
            listedInExposing
                |> filter (not . inDocs)

        commentsFromReorderedVars =
            listedInExposing
                |> filter inDocs
                |> fmap (\(C (pre, post) _) -> pre ++ post)
                |> concat
    in
    if List.null listedInDocs && forceMultiline
        then ( fmap (\x -> [x]) remainingFromExposing, commentsFromReorderedVars )
        else ( listedInDocs ++ if List.null remainingFromExposing then [] else [ remainingFromExposing ], commentsFromReorderedVars )


formatModuleHeader :: Coapplicative annf => ElmVersion -> Bool -> AST.Module.Module [UppercaseIdentifier] (ASTNS annf [UppercaseIdentifier] 'TopLevelNK) -> [Block]
formatModuleHeader elmVersion addDefaultHeader modu =
  let
      maybeHeader =
        if addDefaultHeader
            then Just (AST.Module.header modu |> Maybe.fromMaybe AST.Module.defaultHeader)
            else AST.Module.header modu

      refName (VarRef _ (LowercaseIdentifier name)) = name
      refName (TagRef _ (UppercaseIdentifier name)) = name
      refName (OpRef (SymbolIdentifier name)) = name

      varName (C _ (AST.Listing.Value (LowercaseIdentifier name))) = name
      varName (C _ (AST.Listing.OpValue (SymbolIdentifier name))) = name
      varName (C _ (AST.Listing.Union (C _ (UppercaseIdentifier name)) _)) = name

      documentedVars :: [[String]]
      documentedVars =
          AST.Module.docs modu
              |> extract
              |> fmap Foldable.toList
              |> Maybe.fromMaybe []
              |> concatMap extractDocs

      documentedVarsSet :: Set String
      documentedVarsSet = Set.fromList $ concat documentedVars

      extractDocs block =
          case block of
              Markdown.ElmDocs vars ->
                  fmap (fmap (refName . textToRef)) vars
              _ -> []

      textToRef :: Text -> Ref [UppercaseIdentifier]
      textToRef text =
          case Text.unpack text of
              s@(c:_) | Char.isUpper c -> TagRef [] (UppercaseIdentifier s)
              s@(c:_) | Char.isLower c -> VarRef [] (LowercaseIdentifier s)
              ['(', a, ')'] -> OpRef (SymbolIdentifier [a])
              ['(', a, b, ')'] -> OpRef (SymbolIdentifier [a, b])
              s -> VarRef [] (LowercaseIdentifier s)

      definedVars :: Set (C2 before after AST.Listing.Value)
      definedVars =
          AST.Module.body modu
              |> (extract . I.unFix)
              |> (\(TopLevel decls) -> decls)
              |> concatMap extractVarName
              |> fmap (C ([], []))
              |> Set.fromList

      exportsList =
          case
              AST.Module.exports (maybeHeader |> Maybe.fromMaybe AST.Module.defaultHeader)
          of
              Just (C _ e) -> e
              Nothing -> AST.Listing.ClosedListing

      detailedListingToSet :: AST.Listing.Listing AST.Module.DetailedListing -> Set (C2 before after AST.Listing.Value)
      detailedListingToSet (AST.Listing.OpenListing _) = Set.empty
      detailedListingToSet AST.Listing.ClosedListing = Set.empty
      detailedListingToSet (AST.Listing.ExplicitListing (AST.Module.DetailedListing values operators types) _) =
          Set.unions @[]
              [ Map.assocs values |> fmap (\(name, C c ()) -> C c (AST.Listing.Value name)) |> Set.fromList
              , Map.assocs operators |> fmap (\(name, C c ()) -> C c (AST.Listing.OpValue name)) |> Set.fromList
              , Map.assocs types |> fmap (\(name, C c (C preListing listing)) -> C c (AST.Listing.Union (C preListing name) listing)) |> Set.fromList
              ]

      detailedListingIsMultiline :: AST.Listing.Listing a -> Bool
      detailedListingIsMultiline (AST.Listing.ExplicitListing _ isMultiline) = isMultiline
      detailedListingIsMultiline _ = False

      varsToExpose =
          case AST.Module.exports =<< maybeHeader of
              Nothing ->
                  if all null documentedVars
                      then definedVars
                      else definedVars |> Set.filter (\v -> Set.member (varName v) documentedVarsSet)
              Just (C _ e) -> detailedListingToSet e

      sortedExports =
          sortVars
              (detailedListingIsMultiline exportsList)
              varsToExpose
              documentedVars

      extractVarName :: Coapplicative annf => TopLevelStructure (ASTNS annf ns 'TopLevelDeclarationNK) -> [AST.Listing.Value]
      extractVarName decl =
          case fmap (extract . I.unFix) decl of
              DocComment _ -> []
              BodyComment _ -> []
              Entry (PortAnnotation (C _ (LowercaseIdentifier name)) _ _) -> [ AST.Listing.Value (LowercaseIdentifier name) ]
              Entry (CommonDeclaration def) ->
                case extract $ I.unFix def of
                    Definition pat _ _ _ ->
                        case extract $ I.unFix pat of
                            VarPattern (LowercaseIdentifier name) -> [ AST.Listing.Value (LowercaseIdentifier name) ]
                            RecordPattern fields -> fmap (AST.Listing.Value . extract) fields
                            _ -> []
                    _ -> []
              Entry (Datatype (C _ (NameWithArgs (UppercaseIdentifier name) _)) _) -> [ AST.Listing.Union (C [] (UppercaseIdentifier name)) (AST.Listing.OpenListing (C ([], []) ()))]
              Entry (TypeAlias _ (C _ (NameWithArgs (UppercaseIdentifier name) _)) _) -> [ AST.Listing.Union (C [] (UppercaseIdentifier name)) AST.Listing.ClosedListing ]
              Entry _ -> []

      formatModuleLine' header@(AST.Module.Header srcTag name moduleSettings exports) =
        let
            (preExposing, postExposing) =
                case exports of
                    Nothing -> ([], [])
                    Just (C (pre, post) _) -> (pre, post)
        in
        case elmVersion of
          Elm_0_16 ->
            formatModuleLine_0_16 header

          Elm_0_17 ->
            formatModuleLine elmVersion sortedExports srcTag name moduleSettings preExposing postExposing

          Elm_0_18 ->
            formatModuleLine elmVersion sortedExports srcTag name moduleSettings preExposing postExposing

          Elm_0_19 ->
              formatModuleLine elmVersion sortedExports srcTag name moduleSettings preExposing postExposing

      docs =
          fmap (formatDocComment elmVersion (ImportInfo.fromModule mempty modu)) $ extract $ AST.Module.docs modu

      imports =
          formatImports elmVersion modu
  in
  List.intercalate [ blankLine ] $ concat @[]
      [ maybeToList $ fmap (return . formatModuleLine') maybeHeader
      , maybeToList $ fmap return docs
      , if null imports
          then []
          else [ imports ]
      ]


formatImports :: ElmVersion -> AST.Module.Module [UppercaseIdentifier] decl -> [Block]
formatImports elmVersion modu =
    let
        (C comments imports) =
            AST.Module.imports modu
    in
    [ formatComments comments
        |> maybeToList
    , imports
        |> Map.assocs
        |> fmap (\(name, C pre method) -> formatImport elmVersion (C pre name, method))
    ]
        |> List.filter (not . List.null)
        |> List.intersperse [blankLine]
        |> concat


formatModuleLine_0_16 :: AST.Module.Header -> Block
formatModuleLine_0_16 header =
  let
    elmVersion = Elm_0_16

    exports =
        case AST.Module.exports header of
            Just (C _ value) -> value
            Nothing -> AST.Listing.OpenListing (C ([], []) ())

    formatExports =
        case formatListing (formatDetailedListing elmVersion) exports of
            Just listing ->
                listing
            _ ->
                pleaseReport "UNEXPECTED MODULE DECLARATION" "empty listing"

    whereComments =
        case AST.Module.exports header of
            Nothing -> ([], [])
            Just (C (pre, post) _) -> (pre, post)
  in
  spaceSepOrIndented
      [ line $ keyword "module"
      , formatCommented $ line . formatQualifiedUppercaseIdentifier elmVersion <$> AST.Module.name header
      , formatExports
      , formatCommented (C whereComments $ line $ keyword "where")
      ]


formatModuleLine ::
    ElmVersion
    -> ([[C2 before after AST.Listing.Value]], Comments)
    -> AST.Module.SourceTag
    -> C2 before after [UppercaseIdentifier]
    -> Maybe (C2 before after AST.Module.SourceSettings)
    -> Comments
    -> Comments
    -> Block
formatModuleLine elmVersion (varsToExpose, extraComments) srcTag name moduleSettings preExposing postExposing =
  let
    tag =
      case srcTag of
        AST.Module.Normal ->
          line $ keyword "module"

        AST.Module.Port comments ->
          spaceSepOrIndented
              [ formatTailCommented (C comments $ line $ keyword "port")
              , line $ keyword "module"
              ]

        AST.Module.Effect comments ->
          spaceSepOrIndented
            [ formatTailCommented (C comments $ line $ keyword "effect")
            , line $ keyword "module"
            ]

    exports =
          case varsToExpose of
              [] -> line $ keyword "(..)"
              [oneGroup] ->
                  oneGroup
                      |> fmap (formatCommented . fmap (formatVarValue elmVersion))
                      |> ElmStructure.group' False "(" "," (maybeToList (formatComments extraComments)) ")" False
              _ ->
                  varsToExpose
                      |> fmap (formatCommented . fmap (ElmStructure.group False "" "," "" False . fmap (formatVarValue elmVersion)) . sequenceA)
                      |> ElmStructure.group' False "(" "," (maybeToList (formatComments extraComments)) ")" True

    formatSetting (k, v) =
      formatRecordPair elmVersion "=" (line . formatUppercaseIdentifier elmVersion) (k, v, False)

    formatSettings settings =
      map formatSetting settings
        |> ElmStructure.group True "{" "," "}" False

    whereClause =
      moduleSettings
        |> fmap (formatKeywordCommented "where" . fmap formatSettings)
        |> fmap (\x -> [x])
        |> Maybe.fromMaybe []

    nameClause =
      spaceSepOrIndented
        [ tag
        , formatCommented $ fmap (line . formatQualifiedUppercaseIdentifier elmVersion) name
        ]
  in
  spaceSepOrIndented
      [ spaceSepOrIndented $
          nameClause
          :| whereClause
          ++ pure (formatCommented (C (preExposing, postExposing) $ line $ keyword "exposing"))
      , exports
      ]


formatModule :: Coapplicative annf => ElmVersion -> Bool -> Int -> AST.Module.Module [UppercaseIdentifier] (ASTNS annf [UppercaseIdentifier] 'TopLevelNK) -> Block
formatModule elmVersion addDefaultHeader spacing modu =
    let
        initialComments' =
          case AST.Module.initialComments modu of
            [] ->
              []
            comments ->
              fmap formatComment comments
                ++ [ blankLine, blankLine ]

        spaceBeforeBody =
            case extract $ I.unFix $ AST.Module.body modu of
                TopLevel [] -> 0
                TopLevel (BodyComment _ : _) -> spacing + 1
                TopLevel _ -> spacing

        decls =
          case extract $ I.unFix $ AST.Module.body modu of
              TopLevel decls -> decls
    in
      stack1 $
          concat @[]
              [ initialComments'
              , formatModuleHeader elmVersion addDefaultHeader modu
              , List.replicate spaceBeforeBody blankLine
              , maybeToList $ formatModuleBody spacing elmVersion (ImportInfo.fromModule mempty modu) decls
              ]


formatModuleBody :: forall annf. Coapplicative annf => Int -> ElmVersion -> ImportInfo [UppercaseIdentifier] -> [TopLevelStructure (ASTNS annf [UppercaseIdentifier] 'TopLevelDeclarationNK)] -> Maybe Block
formatModuleBody linesBetween elmVersion importInfo body =
    let
        entryType :: ASTNS annf ns 'TopLevelDeclarationNK -> BodyEntryType
        entryType adecl =
            case extract $ I.unFix adecl of
                CommonDeclaration def ->
                    case extract $ I.unFix def of
                        Definition pat _ _ _ ->
                            case extract $ I.unFix pat of
                                VarPattern name ->
                                    BodyNamed $ VarRef () name

                                OpPattern name ->
                                    BodyNamed $ OpRef name

                                _ ->
                                    BodyUnnamed

                        TypeAnnotation (C _ name) _ ->
                            BodyNamed name

                Datatype (C _ (NameWithArgs name _)) _ ->
                    BodyNamed $ TagRef () name

                TypeAlias _ (C _ (NameWithArgs name _)) _ ->
                    BodyNamed $ TagRef () name

                PortDefinition_until_0_16 (C _ name) _ _ ->
                    BodyNamed $ VarRef () name

                PortAnnotation (C _ name) _ _ ->
                    BodyNamed $ VarRef () name

                Fixity_until_0_18 _ _ _ _ _ ->
                    BodyFixity

                Fixity _ _ _ _ ->
                    BodyFixity
    in
    formatTopLevelBody linesBetween elmVersion importInfo $
        fmap (fmap $ \b -> (entryType b, formatDeclaration elmVersion importInfo b)) body


data BodyEntryType
    = BodyNamed (Ref ())
    | BodyUnnamed
    | BodyFixity


formatTopLevelBody ::
    Int
    -> ElmVersion
    -> ImportInfo [UppercaseIdentifier]
    -> [TopLevelStructure (BodyEntryType, Block)]
    -> Maybe Block
formatTopLevelBody linesBetween elmVersion importInfo body =
    let
        extraLines n =
            List.replicate n blankLine

        spacer a b =
            case (declarationType (fmap fst a), declarationType (fmap fst b)) of
                (DStarter, _) -> 0
                (_, DCloser) -> 0
                (DComment, DComment) -> 0
                (_, DComment) -> if linesBetween == 1 then 1 else linesBetween + 1
                (DComment, DDefinition _) -> if linesBetween == 1 then 0 else linesBetween
                (DComment, _) -> linesBetween
                (DDocComment, DDefinition _) -> 0
                (DDefinition Nothing, DDefinition (Just _)) -> linesBetween
                (DDefinition _, DStarter) -> linesBetween
                (DDefinition Nothing, DDefinition Nothing) -> linesBetween
                (DDefinition a, DDefinition b) ->
                    if a == b
                        then 0
                        else linesBetween
                (DCloser, _) -> linesBetween
                (_, DDocComment) -> linesBetween
                (DDocComment, DStarter) -> 0
                (DFixity, DFixity) -> 0
                (DFixity, _) -> linesBetween
                (_, DFixity) -> linesBetween

        boxes =
            intersperseMap (\a b -> extraLines $ spacer a b)
                (formatTopLevelStructure elmVersion importInfo . fmap snd)
                body
    in
        case boxes of
            [] -> Nothing
            _ -> Just $ stack1 boxes


data ElmCodeBlock annf ns
    = DeclarationsCode [TopLevelStructure (ASTNS annf ns 'TopLevelDeclarationNK)]
    | ExpressionsCode [TopLevelStructure (C0Eol (ASTNS annf ns 'ExpressionNK))]
    | ModuleCode (AST.Module.Module ns (ASTNS annf ns 'TopLevelNK))

convertElmCodeBlock :: Functor ann => (forall x. ann x -> ann' x) -> ElmCodeBlock ann ns -> ElmCodeBlock ann' ns
convertElmCodeBlock f = \case
    DeclarationsCode decls -> DeclarationsCode (fmap (fmap $ I.convert f) decls)
    ExpressionsCode exprs -> ExpressionsCode (fmap (fmap $ fmap $ I.convert f) exprs)
    ModuleCode mod -> ModuleCode (fmap (I.convert f) mod)


-- TODO: there must be an existing haskell function that does this, right?
firstOf :: [a -> Maybe b] -> a -> Maybe b
firstOf options value =
    case options of
        [] -> Nothing
        (next:rest) ->
            case next value of
                Just result -> Just result
                Nothing -> firstOf rest value


formatDocComment :: ElmVersion -> ImportInfo [UppercaseIdentifier] -> Markdown.Blocks -> Block
formatDocComment elmVersion importInfo blocks =
    let
        parse :: String -> Maybe (ElmCodeBlock Identity [UppercaseIdentifier])
        parse source =
            source
                |> firstOf
                    [ fmap DeclarationsCode . Result.toMaybe . Parse.parseDeclarations elmVersion
                    , fmap ExpressionsCode . Result.toMaybe . Parse.parseExpressions elmVersion
                    , fmap ModuleCode . Result.toMaybe . Parse.parseModule elmVersion
                    ]
                |> fmap (convertElmCodeBlock (pure . extract))

        format ::
            (Applicative annf, Coapplicative annf) =>
            ElmCodeBlock annf [UppercaseIdentifier] -> String
        format result =
            case result of
                ModuleCode modu ->
                    formatModule elmVersion False 1 modu
                        |> (Text.unpack . Box.render)

                DeclarationsCode declarations ->
                    formatModuleBody 1 elmVersion importInfo declarations
                        |> fmap (Text.unpack . Box.render)
                        |> fromMaybe ""

                ExpressionsCode expressions ->
                    expressions
                        |> fmap (fmap $ fmap $ I.convert (Identity . extract))
                        |> fmap (fmap $ formatEolCommented . fmap (syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo))
                        |> fmap (fmap $ (,) BodyUnnamed)
                        |> formatTopLevelBody 1 elmVersion importInfo
                        |> fmap (Text.unpack . Box.render)
                        |> fromMaybe ""

        content :: String
        content =
            ElmFormat.Render.Markdown.formatMarkdown (fmap format . parse) $ fmap cleanBlock blocks

        cleanBlock :: Markdown.Block -> Markdown.Block
        cleanBlock block =
            case block of
                Markdown.ElmDocs docs ->
                    Markdown.ElmDocs $
                        (fmap . fmap)
                            (Text.replace (Text.pack "(..)") (Text.pack ""))
                            docs
                _ ->
                    block
    in
    formatDocCommentString content


formatDocCommentString :: String -> Block
formatDocCommentString docs =
    case lines docs of
        [] ->
            line $ punc "{-|" <> space <> punc "-}"
        [first] ->
            Block.stack
                [ line $ punc "{-|" <> space <> literal first
                , line $ punc "-}"
                ]
        (first:rest) ->
            Block.stack $ NonEmpty.fromList $ mconcat
                [ pure $ line $ punc "{-|" <> space <> literal first
                , fmap (line . literal) rest
                , pure $ line $ punc "-}"
                ]


formatImport :: ElmVersion -> AST.Module.UserImport -> Block
formatImport elmVersion (name@(C _ rawName), method) =
    let
        normalizedAs =
            case AST.Module.alias method of
                Just (C _ requestedAs) | [requestedAs] == rawName -> Nothing
                other -> other

        exposing = AST.Module.exposedVars method
    in
    spaceSepOrIndented $ 
        Block.prefixOrIndent (Just space)
            (keyword "import")
            (formatName name)
        :| catMaybes
            [ formatAsClause =<< normalizedAs
            , formatExposingClause exposing
            ]
    where
        formatName = formatPreCommented . fmap (line . formatQualifiedUppercaseIdentifier elmVersion)

        formatAsClause =
            formatImportClause "as"
                (Just . line . formatUppercaseIdentifier elmVersion)

        formatExposingClause =
            formatImportClause "exposing"
                (formatListing (formatDetailedListing elmVersion))

        formatImportClause :: String -> (a -> Maybe Block) -> C2 beforeKeyword afterKeyword a -> Maybe Block
        formatImportClause keyw format input =
          case fmap format input of
            C ([], []) Nothing ->
              Nothing

            C (preKeyword, postKeyword) (Just listing') ->
                Just $ spaceSepOrIndented
                    [ formatPreCommented (C preKeyword $ line $ keyword keyw)
                    , formatPreCommented (C postKeyword listing')
                    ]

            _ ->
              Just $ pleaseReport "UNEXPECTED IMPORT" "import clause comments with no clause"


formatListing :: (a -> [Block]) -> AST.Listing.Listing a -> Maybe Block
formatListing format listing =
    case listing of
        AST.Listing.ClosedListing ->
            Nothing

        AST.Listing.OpenListing (C comments ()) ->
            Just $ parens $ formatCommented $ C comments $ line $ keyword ".."

        AST.Listing.ExplicitListing vars multiline ->
            case format vars of
                [] -> Nothing
                vars' -> Just $ ElmStructure.group False "(" "," ")" multiline vars'


formatDetailedListing :: ElmVersion -> AST.Module.DetailedListing -> [Block]
formatDetailedListing elmVersion listing =
    concat @[]
        [ formatCommentedMap
            (\name () -> AST.Listing.OpValue name)
            (formatVarValue elmVersion)
            (AST.Module.operators listing)
        , formatCommentedMap
            (\name (C inner listing_) -> AST.Listing.Union (C inner name) listing_)
            (formatVarValue elmVersion)
            (AST.Module.types listing)
        , formatCommentedMap
            (\name () -> AST.Listing.Value name)
            (formatVarValue elmVersion)
            (AST.Module.values listing)
        ]


formatCommentedMap :: (k -> v -> a) -> (a -> Block) ->  AST.Listing.CommentedMap k v -> [Block]
formatCommentedMap construct format values =
    let
        format' (k, C c v)
            = formatCommented $ C c (format $ construct k v)
    in
    values
        |> Map.assocs
        |> fmap format'


formatVarValue :: ElmVersion -> AST.Listing.Value -> Block
formatVarValue elmVersion aval =
    case aval of
        AST.Listing.Value val ->
            line $ formatLowercaseIdentifier elmVersion [] val

        AST.Listing.OpValue (SymbolIdentifier name) ->
            line $ identifier $ "(" ++ name ++ ")"

        AST.Listing.Union (C postName name) listing' ->
            let
                listing =
                    case (ElmVersion.syntax_0_19_disallowExplicitUnionListings elmVersion, listing') of
                        (True, ExplicitListing _ _) -> OpenListing (C ([], []) ())
                        _ -> listing'

                comments = formatComments postName

                spacer =
                    case comments of
                        Just _ -> Just space
                        Nothing -> Nothing

                listingClause =
                    case (formatComments postName, formatListing' listing) of
                        (Nothing, Nothing) -> []
                        (Just c, Nothing) -> pure c
                        (Nothing, Just l) -> pure l
                        (Just c, Just l) -> pure $ spaceSepOrStack [c, l]
            in
            Block.rowOrIndent spacer $
                formatName name
                :| listingClause
            where
                formatName = line . formatUppercaseIdentifier elmVersion

                formatListing' =
                    formatListing $
                        formatCommentedMap
                            (\name_ () -> name_)
                            (line . formatUppercaseIdentifier elmVersion)


formatTopLevelStructure :: ElmVersion -> ImportInfo [UppercaseIdentifier] -> TopLevelStructure Block -> Block
formatTopLevelStructure elmVersion importInfo topLevelStructure =
    case topLevelStructure of
        DocComment docs ->
            formatDocComment elmVersion importInfo docs

        BodyComment c ->
            formatComment c

        Entry entry ->
            entry


formatCommonDeclaration ::
    Coapplicative annf =>
    ElmVersion -> ImportInfo [UppercaseIdentifier] -> ASTNS annf [UppercaseIdentifier] 'CommonDeclarationNK -> Block
formatCommonDeclaration elmVersion importInfo decl =
    case extract $ I.unFix $ I.convert (Identity . extract) decl of
        Definition name args comments expr ->
            formatDefinition elmVersion importInfo name args comments expr

        TypeAnnotation name typ ->
            formatTypeAnnotation elmVersion name typ


-- | temporary during conversion to Block API:
-- like `allSingles`, but does not give Lines if mustBreak is True
allSingles' :: [Block] -> Either [Block] [Line]
allSingles' blocks =
  case allSingles blocks of
    Right (lines, False) -> Right lines
    _ -> Left blocks


formatDeclaration ::
    Coapplicative annf =>
    ElmVersion -> ImportInfo [UppercaseIdentifier] -> ASTNS annf [UppercaseIdentifier] 'TopLevelDeclarationNK -> Block
formatDeclaration elmVersion importInfo decl =
    case extract $ I.unFix $ I.convert (Identity . extract) decl of
        CommonDeclaration def ->
            formatCommonDeclaration elmVersion importInfo def

        Datatype nameWithArgs tags ->
            case formatOpenCommentedList $ formatCtor <$> tags of
                [] -> error "List can't be empty"
                first:rest ->
                    Block.stack
                        [ spaceSepOrIndented
                            [ line $ keyword "type"
                            , formatCommented $ formatNameWithArgs elmVersion <$> nameWithArgs
                            ]
                        , indent $ Block.stack $ NonEmpty.fromList $ mconcat
                            [ pure $ prefix (row [punc "=", space]) first
                            , fmap (prefix (row [punc "|", space])) rest
                            ]
                        ]
            where
                formatCtor (NameWithArgs tag args') =
                    spaceSepOrIndented $
                        line (formatUppercaseIdentifier elmVersion tag)
                        :| (formatArg <$> args')

                formatArg =
                    formatPreCommented . fmap (typeParens ForCtor . formatType elmVersion)

        TypeAlias preAlias nameWithArgs typ ->
            ElmStructure.definition "=" True
            (line $ keyword "type")
            [ formatPreCommented (C preAlias $ line $ keyword "alias")
            , formatCommented $ fmap (formatNameWithArgs elmVersion) nameWithArgs
            ]
            (formatPreCommentedStack $ fmap (typeParens NotRequired . formatType elmVersion) typ)

        PortAnnotation name typeComments typ ->
            ElmStructure.definition ":" False
            (line $ keyword "port")
            [ formatCommented $ fmap (line . formatLowercaseIdentifier elmVersion []) name ]
            (formatCommented' typeComments $ typeParens NotRequired $ formatType elmVersion typ)

        PortDefinition_until_0_16 name bodyComments expr ->
            ElmStructure.definition "=" True
            (line $ keyword "port")
            [formatCommented $ fmap (line . formatLowercaseIdentifier elmVersion []) name]
            (formatCommented' bodyComments $ syntaxParens SyntaxSeparated $ formatExpression elmVersion importInfo expr)

        Fixity_until_0_18 assoc precedenceComments precedence nameComments name ->
            case
                ( formatCommented' nameComments $ line $ formatInfixVar elmVersion name
                , formatCommented' precedenceComments $ line $ literal $ show precedence
                )
            of
                (SingleLine name', SingleLine precedence') ->
                    line $ row
                        [ case assoc of
                                L -> keyword "infixl"
                                R -> keyword "infixr"
                                N -> keyword "infix"
                        , space
                        , precedence'
                        , space
                        , name'
                        ]
                _ ->
                    pleaseReport "TODO" "multiline fixity declaration"

        Fixity assoc precedence name value ->
            let
                formatAssoc a =
                    case a of
                        L -> keyword "left "
                        R -> keyword "right"
                        N -> keyword "non  "
            in
            spaceSepOrIndented
                [ line $ keyword "infix"
                , formatPreCommented $ fmap (line . formatAssoc) assoc
                , formatPreCommented $ fmap (line . literal . show) precedence
                , formatCommented $ fmap (line . formatSymbolIdentifierInParens) name
                , line $ keyword "="
                , formatPreCommented $ fmap (line . identifier . formatVarName elmVersion) value
                ]


formatNameWithArgs :: ElmVersion -> NameWithArgs UppercaseIdentifier LowercaseIdentifier -> Block
formatNameWithArgs elmVersion (NameWithArgs name args) =
    spaceSepOrIndented $
        line (formatUppercaseIdentifier elmVersion name)
        :| (formatArg <$> args)
    where
        formatArg = formatPreCommented . fmap (line . formatLowercaseIdentifier elmVersion [])


formatDefinition ::
    ElmVersion
    -> ImportInfo [UppercaseIdentifier]
    -> ASTNS Identity [UppercaseIdentifier] 'PatternNK
    -> [C1 before (ASTNS Identity [UppercaseIdentifier] 'PatternNK)]
    -> Comments
    -> ASTNS Identity [UppercaseIdentifier] 'ExpressionNK
    -> Block
formatDefinition elmVersion importInfo name args comments expr =
  let
    body =
      stack1 $ concat @[]
        [ map formatComment comments
        , [ syntaxParens SyntaxSeparated $ formatExpression elmVersion importInfo expr ]
        ]
  in
    ElmStructure.definition "=" True
      (syntaxParens SpaceSeparated $ formatPattern elmVersion name)
      (map (\(C x y) -> formatCommented' x $ syntaxParens SpaceSeparated $ formatPattern elmVersion y) args)
      body


formatTypeAnnotation ::
    Coapplicative annf =>
    ElmVersion -> C1 after (Ref ()) -> C1 before (ASTNS annf [UppercaseIdentifier] 'TypeNK) -> Block
formatTypeAnnotation elmVersion name typ =
  ElmStructure.definition ":" False
    (formatTailCommented $ fmap (line . formatVar elmVersion . fmap (\() -> [])) name)
    []
    (formatPreCommented $ fmap (typeParens NotRequired . formatType elmVersion) typ)


formatPattern ::
    Coapplicative annf =>
    ElmVersion -> ASTNS annf [UppercaseIdentifier] 'PatternNK -> (SyntaxContext, Block)
formatPattern elmVersion apattern =
    case extract $ I.unFix apattern of
        Anything ->
            (,) SyntaxSeparated $ line $ keyword "_"

        UnitPattern comments ->
            (,) SyntaxSeparated $ formatUnit '(' ')' comments

        LiteralPattern lit ->
            (,) SyntaxSeparated $ formatLiteral elmVersion lit

        VarPattern var ->
            (,) SyntaxSeparated $ line $ formatLowercaseIdentifier elmVersion [] var

        OpPattern (SymbolIdentifier name) ->
            (,) SyntaxSeparated $ line $ identifier $ "(" ++ name ++ ")"

        ConsPattern first rest ->
            let
                formatRight (C (preOp, postOp, eol) term) =
                    ( False
                    , preOp
                    , "::"
                    , formatC2Eol $
                        (fmap $ syntaxParens SpaceSeparated . formatPattern elmVersion)
                        (C (postOp, [], eol) term)
                    )
            in
                (,) SpaceSeparated $
                formatBinary False
                    (formatEolCommented $ fmap (syntaxParens SpaceSeparated . formatPattern elmVersion) first)
                    (formatRight <$> toCommentedList rest)

        DataPattern (ns, tag) [] ->
            let
                ctor = ns ++ [tag]
            in
            line (formatQualifiedUppercaseIdentifier elmVersion ctor)
                |>
                    case (elmVersion, ctor) of
                        (Elm_0_16, [_]) ->
                            (,) SyntaxSeparated
                        (Elm_0_16, _) ->
                            (,) SpaceSeparated
                        _ ->
                            (,) SyntaxSeparated

        DataPattern (ns, tag) patterns ->
            let
                ctor = ns ++ [tag]
            in
            (,) SpaceSeparated $
            ElmStructure.application
                (FAJoinFirst JoinAll)
                (line $ formatQualifiedUppercaseIdentifier elmVersion ctor)
                (fmap (formatPreCommented . fmap (syntaxParens SpaceSeparated . formatPattern elmVersion)) patterns)

        PatternParens pattern ->
            formatCommented (fmap (syntaxParens SyntaxSeparated . formatPattern elmVersion) pattern)
              |> parens
              |> (,) SyntaxSeparated

        TuplePattern patterns ->
            (,) SyntaxSeparated $
            ElmStructure.group True "(" "," ")" False $ fmap (formatCommented . fmap (syntaxParens SyntaxSeparated . formatPattern elmVersion)) patterns

        EmptyListPattern comments ->
            (,) SyntaxSeparated $
            formatUnit '[' ']' comments

        ListPattern patterns ->
            (,) SyntaxSeparated $
            ElmStructure.group True "[" "," "]" False $ fmap (formatCommented . fmap (syntaxParens SyntaxSeparated . formatPattern elmVersion)) patterns

        EmptyRecordPattern comments ->
            (,) SyntaxSeparated $
            formatUnit '{' '}' comments

        RecordPattern fields ->
            (,) SyntaxSeparated $
            ElmStructure.group True "{" "," "}" False $ map (formatCommented . fmap (line . formatLowercaseIdentifier elmVersion [])) fields

        Alias pattern name ->
          (,) SpaceSeparated $
          case
            ( formatTailCommented $ fmap (syntaxParens SpaceSeparated . formatPattern elmVersion) pattern
            , formatPreCommented $ fmap (line . formatLowercaseIdentifier elmVersion []) name
            )
          of
            (SingleLine pattern', SingleLine name') ->
              line $ row
                [ pattern'
                , space
                , keyword "as"
                , space
                , name'
                ]

            (pattern', name') ->
              stack1
                [ pattern'
                , line $ keyword "as"
                , indent name'
                ]


formatRecordPair :: ElmVersion -> String -> (v -> Block) -> (C2 before after LowercaseIdentifier, C2 before after v, Bool) -> Block
formatRecordPair elmVersion delim formatValue (C (pre, postK) k, v, forceMultiline) =
    ElmStructure.equalsPair delim forceMultiline
      (formatCommented $ line . formatLowercaseIdentifier elmVersion [] <$> C ([], postK) k)
      (formatCommented $ fmap formatValue v)
    |> C pre
    |> formatPreCommented


formatPair :: String -> Pair Line Block -> Block
formatPair delim (Pair a b (ForceMultiline forceMultiline)) =
    ElmStructure.equalsPair delim forceMultiline
        (formatTailCommented $ fmap line a)
        (formatPreCommented b)


negativeCasePatternWorkaround ::
    Coapplicative annf =>
    ASTNS annf [UppercaseIdentifier] 'PatternNK -> Block -> Block
negativeCasePatternWorkaround pattern =
    case extract $ I.unFix pattern of
        LiteralPattern (IntNum i _) | i < 0 -> parens
        LiteralPattern (FloatNum f _) | f < 0 -> parens
        _ -> id


data SyntaxContext
    = SyntaxSeparated
    | InfixSeparated
    | SpaceSeparated
    | AmbiguousEnd


syntaxParens :: SyntaxContext -> (SyntaxContext, Block) -> Block
syntaxParens outer (inner, box) =
    parensIf (needsParensInContext inner outer) box
    where
        parensIf True = parens
        parensIf False = id


needsParensInContext :: SyntaxContext -> SyntaxContext -> Bool
needsParensInContext inner outer =
    case (inner, outer) of
        (SpaceSeparated, SpaceSeparated) -> True
        (InfixSeparated, SpaceSeparated) -> True
        (InfixSeparated, InfixSeparated) -> True
        (AmbiguousEnd, SpaceSeparated) -> True
        (AmbiguousEnd, InfixSeparated) -> True
        (InfixSeparated, AmbiguousEnd) -> True
        _ -> False


formatExpression ::
    ElmVersion -> ImportInfo [UppercaseIdentifier]
    -> ASTNS Identity [UppercaseIdentifier] 'ExpressionNK
    -> (SyntaxContext, Block)
formatExpression elmVersion importInfo aexpr =
    case extract $ I.unFix aexpr of
        Literal lit ->
            (,) SyntaxSeparated $ formatLiteral elmVersion lit

        VarExpr v ->
            (,) SyntaxSeparated $ line $ formatVar elmVersion v

        Range left right multiline ->
            case elmVersion of
                Elm_0_16 -> (,) SyntaxSeparated $ formatRange_0_17 elmVersion importInfo left right multiline
                Elm_0_17 -> (,) SyntaxSeparated $ formatRange_0_17 elmVersion importInfo left right multiline
                Elm_0_18 -> formatRange_0_18 elmVersion importInfo left right
                Elm_0_19 -> formatRange_0_18 elmVersion importInfo left right

        ExplicitList exprs trailing multiline ->
            (,) SyntaxSeparated $
            formatSequence '[' ',' (Just ']')
                multiline
                trailing
                (syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo <$> exprs)

        Binops left ops multiline ->
            (,) InfixSeparated $
            formatBinops elmVersion importInfo left ops multiline

        Lambda patterns bodyComments expr multiline ->
            (,) AmbiguousEnd $
            case
                ( multiline
                , allSingles $ fmap (formatPreCommented . fmap (syntaxParens SpaceSeparated . formatPattern elmVersion)) patterns
                , List.null bodyComments
                , syntaxParens SyntaxSeparated $ formatExpression elmVersion importInfo expr
                )
            of
                (False, Right (patterns', isMustBreak), True, SingleLine expr') ->
                    line $ row
                        [ punc "\\"
                        , row $ List.intersperse space patterns'
                        , space
                        , punc "->"
                        , space
                        , expr'
                        ]
                (_, Right (patterns', isMustBreak), _, expr') ->
                    stack1
                        [ line $ row
                            [ punc "\\"
                            , row $ List.intersperse space patterns'
                            , space
                            , punc "->"
                            ]
                        , indent $ stack1 $
                            fmap formatComment bodyComments
                            ++ [ expr' ]
                        ]
                (_, Left [], _, _) ->
                    pleaseReport "UNEXPECTED LAMBDA" "no patterns"
                (_, Left patterns', _, expr') ->
                    stack1
                        [ prefix (punc "\\") $ stack1 patterns'
                        , line $ punc "->"
                        , indent $ stack1 $
                            fmap formatComment bodyComments
                            ++ [ expr' ]
                        ]

        Unary Negative e ->
            (,) SyntaxSeparated $
            prefix (punc "-") $ syntaxParens SpaceSeparated $ formatExpression elmVersion importInfo e -- TODO: This might need something stronger than SpaceSeparated?

        App left [] _ ->
            formatExpression elmVersion importInfo left

        App left args multiline ->
            (,) SpaceSeparated $
            ElmStructure.application
                multiline
                (syntaxParens InfixSeparated $ formatExpression elmVersion importInfo left)
                (fmap (formatPreCommentedExpression elmVersion importInfo SpaceSeparated) args)

        If if' elseifs (C elsComments els) ->
            let
                opening key cond =
                    case (key, cond) of
                        (SingleLine key', SingleLine cond') ->
                            line $ row
                                [ key'
                                , space
                                , cond'
                                , space
                                , keyword "then"
                                ]
                        _ ->
                            stack1
                                [ key
                                , cond |> indent
                                , line $ keyword "then"
                                ]

                formatIf (IfClause cond body) =
                    stack1
                        [ opening (line $ keyword "if") $ formatCommentedExpression elmVersion importInfo cond
                        , indent $ formatCommented_ True $ fmap (syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo) body
                        ]

                formatElseIf (C ifComments (IfClause cond body)) =
                  let
                    key =
                      case formatPreCommented (C ifComments $ line $ keyword "if") of
                        SingleLine key' ->
                          line $ row [ keyword "else", space, key' ]
                        key' ->
                          stack1
                            [ line $ keyword "else"
                            , key'
                            ]
                  in
                    stack1
                      [ blankLine
                      , opening key $ formatCommentedExpression elmVersion importInfo cond
                      , indent $ formatCommented_ True $ fmap (syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo) body
                      ]
            in
                (,) AmbiguousEnd $
                formatIf if'
                    |> andThen (fmap formatElseIf elseifs)
                    |> andThen
                        [ blankLine
                        , line $ keyword "else"
                        , indent $ formatCommented_ True $ fmap (syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo) (C (elsComments, []) els)
                        ]

        Let defs bodyComments expr ->
            let
                spacer :: AST typeRef ctorRef varRef (I.Fix Identity (AST typeRef ctorRef varRef)) 'LetDeclarationNK -> AST typeRef ctorRef varRef getType 'LetDeclarationNK -> [Block]
                spacer first _ =
                    case first of
                        LetCommonDeclaration (I.Fix (Identity (Definition _ _ _ _))) ->
                            [ blankLine ]
                        _ ->
                            []

                formatDefinition' def =
                  case def of
                    LetCommonDeclaration (I.Fix (Identity (Definition name args comments expr'))) ->
                      formatDefinition elmVersion importInfo name args comments expr'

                    LetCommonDeclaration (I.Fix (Identity (TypeAnnotation name typ))) ->
                      formatTypeAnnotation elmVersion name typ

                    LetComment comment ->
                        formatComment comment
            in
                (,) AmbiguousEnd $ -- TODO: not tested
                line (keyword "let")
                    |> andThen
                        (defs
                            |> fmap (extract . I.unFix)
                            |> intersperseMap spacer formatDefinition'
                            |> map indent
                        )
                    |> andThen
                        [ line $ keyword "in"
                        , stack1 $
                            fmap formatComment bodyComments
                            ++ [syntaxParens SyntaxSeparated $ formatExpression elmVersion importInfo expr]
                        ]

        Case (subject,multiline) clauses ->
            let
                opening =
                  case
                    ( multiline
                    , formatCommentedExpression elmVersion importInfo subject
                    )
                  of
                      (False, SingleLine subject') ->
                          line $ row
                              [ keyword "case"
                              , space
                              , subject'
                              , space
                              , keyword "of"
                              ]
                      (_, subject') ->
                          stack1
                              [ line $ keyword "case"
                              , indent subject'
                              , line $ keyword "of"
                              ]

                clause (CaseBranch prePat postPat preExpr pat expr) =
                    case
                      ( postPat
                      , formatPattern elmVersion pat
                          |> syntaxParens SyntaxSeparated
                          |> negativeCasePatternWorkaround pat
                      , formatCommentedStack (fmap (syntaxParens SyntaxSeparated . formatPattern elmVersion) (C (prePat, postPat) pat))
                          |> negativeCasePatternWorkaround pat
                      , formatPreCommentedStack $ fmap (syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo) (C preExpr expr)
                      )
                    of
                        (_, _, SingleLine pat', body') ->
                            stack1
                                [ line $ row [ pat', space, keyword "->"]
                                , indent body'
                                ]
                        ([], SingleLine pat', _, body') ->
                            stack1 $
                                fmap formatComment prePat
                                ++ [ line $ row [ pat', space, keyword "->"]
                                   , indent body'
                                   ]
                        (_, _, pat', body') ->
                            stack1
                              [ pat'
                              , line $ keyword "->"
                              , indent body'
                              ]
            in
                (,) AmbiguousEnd $ -- TODO: not tested
                opening
                    |> andThen
                        (clauses
                            |> fmap (clause . extract . I.unFix)
                            |> List.intersperse blankLine
                            |> map indent
                        )

        Tuple exprs trailing multiline ->
            (,) SyntaxSeparated $
            formatSequence '(' ',' (Just ')')
                multiline
                trailing
                (syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo <$> exprs)

        TupleFunction n ->
            (,) SyntaxSeparated $
            line $ keyword $ "(" ++ List.replicate (n-1) ',' ++ ")"

        Access expr field ->
            (,) SyntaxSeparated $
            formatExpression elmVersion importInfo expr
                |> syntaxParens SpaceSeparated -- TODO: does this need a different context than SpaceSeparated?
                |> addSuffix (row [punc ".", formatLowercaseIdentifier elmVersion [] field])

        AccessFunction (LowercaseIdentifier field) ->
            (,) SyntaxSeparated $
            line $ identifier $ "." ++ formatVarName' elmVersion field

        Record base fields trailing multiline ->
            (,) SyntaxSeparated $
            formatRecordLike
                (fmap (line . formatLowercaseIdentifier elmVersion []) <$> base)
                (fmap (formatPair "=" . mapPair (formatLowercaseIdentifier elmVersion []) (syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo)) fields)
                trailing multiline

        Parens expr ->
            case expr of
                C ([], []) expr' ->
                    formatExpression elmVersion importInfo expr'

                _ ->
                    (,) SyntaxSeparated $
                    formatCommentedExpression elmVersion importInfo expr
                        |> parens


        Unit comments ->
            (,) SyntaxSeparated $
            formatUnit '(' ')' comments

        GLShader src ->
          (,) SyntaxSeparated $
          line $ row
            [ punc "[glsl|"
            , literal src
            , punc "|]"
            ]


formatCommentedExpression ::
    ElmVersion -> ImportInfo [UppercaseIdentifier]
    -> C2 before after (ASTNS Identity [UppercaseIdentifier] 'ExpressionNK)
    -> Block
formatCommentedExpression elmVersion importInfo (C (pre, post) e) =
    let
        commented' =
            case extract $ I.unFix e of
                Parens (C (pre'', post'') e'') ->
                    C (pre ++ pre'', post'' ++ post) e''
                _ -> C (pre, post) e
    in
    formatCommented $ fmap (syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo) commented'


formatPreCommentedExpression ::
    Coapplicative annf =>
    ElmVersion -> ImportInfo [UppercaseIdentifier] -> SyntaxContext
    -> C1 before (ASTNS annf [UppercaseIdentifier] 'ExpressionNK)
    -> Block
formatPreCommentedExpression elmVersion importInfo context (C pre e) =
    let
        (pre', e') =
            case extract $ I.unFix e of
                Parens (C (pre'', []) e'') ->
                    (pre ++ pre'', e'')
                _ -> (pre, e)
    in
    formatCommented' pre' (syntaxParens context $ formatExpression elmVersion importInfo $ I.convert (Identity . extract) e')


formatRecordLike ::
    Maybe (C2 before after Block) -> Sequence Block -> Comments -> ForceMultiline
    -> Block
formatRecordLike base' fields trailing multiline =
    case (base', fields) of
      ( Just base, pairs' ) ->
          ElmStructure.extensionGroup' '{' '}'
              ((\(ForceMultiline b) -> b) multiline)
              (formatCommented base)
              (formatSequence '|' ',' Nothing
                  multiline
                  trailing
                  pairs')

      ( Nothing, pairs' ) ->
          formatSequence '{' ',' (Just '}')
              multiline
              trailing
              pairs'


formatSequence :: Char -> Char -> Maybe Char -> ForceMultiline -> Comments -> Sequence Block -> Block
formatSequence left delim right (ForceMultiline multiline) trailing (Sequence (first:rest)) =
    let
        formatItem delim_ (C (pre, post, eol) item) =
            maybe id (stack' . stack' blankLine) (formatComments pre) $
            prefix (row [ punc [delim_], space ]) $
            formatC2Eol $ C (post, [], eol) item
    in
        spaceSepOrStackForce multiline $
            rowOrStackForce multiline (
                formatItem left first
                :| fmap (formatItem delim) rest
            )
            :| (maybe [] (flip (:) [] . stack' blankLine) (formatComments trailing) ++ Maybe.maybeToList (fmap (line . punc . flip (:) []) right))
formatSequence left _ (Just right) _ trailing (Sequence []) =
    formatUnit left right trailing
formatSequence left _ Nothing _ trailing (Sequence []) =
    formatUnit left ' ' trailing


mapIsLast :: (Bool -> a -> b) -> [a] -> [b]
mapIsLast _ [] = []
mapIsLast f [last_] = [f True last_]
mapIsLast f (next:rest) = f False next : mapIsLast f rest


formatBinops ::
    ElmVersion
    -> ImportInfo [UppercaseIdentifier]
    -> ASTNS Identity [UppercaseIdentifier] 'ExpressionNK
    -> [BinopsClause (Ref [UppercaseIdentifier]) (ASTNS Identity [UppercaseIdentifier] 'ExpressionNK)]
    -> Bool
    -> Block
formatBinops elmVersion importInfo left ops multiline =
    let
        formatPair_ isLast (BinopsClause po o pe e) =
            let
                isLeftPipe =
                    o == OpRef (SymbolIdentifier "<|")

                formatContext =
                    if isLeftPipe && isLast
                        then AmbiguousEnd
                        else InfixSeparated
            in
            ( isLeftPipe
            , po
            , Text.unpack $ Box.renderLine 0 $ formatInfixVar elmVersion o
            , formatCommented' pe $ syntaxParens formatContext $ formatExpression elmVersion importInfo e
            )
    in
        formatBinary
            multiline
            (syntaxParens InfixSeparated $ formatExpression elmVersion importInfo left)
            (mapIsLast formatPair_ ops)


formatRange_0_17 ::
    ElmVersion -> ImportInfo [UppercaseIdentifier]
    -> C2 before after (ASTNS Identity [UppercaseIdentifier] 'ExpressionNK)
    -> C2 before after (ASTNS Identity [UppercaseIdentifier] 'ExpressionNK)
    -> Bool
    -> Block
formatRange_0_17 elmVersion importInfo left right multiline =
    case
        ( multiline
        , formatCommentedExpression elmVersion importInfo left
        , formatCommentedExpression elmVersion importInfo right
        )
    of
        (False, SingleLine left', SingleLine right') ->
            line $ row
                [ punc "["
                , left'
                , punc ".."
                , right'
                , punc "]"
                ]
        (_, left', right') ->
            stack1
                [ line $ punc "["
                , indent left'
                , line $ punc ".."
                , indent right'
                , line $ punc "]"
                ]

nowhere :: A.Position
nowhere =
    A.Position 0 0


noRegion :: a -> A.Located a
noRegion =
    A.at nowhere nowhere

formatRange_0_18 ::
    Coapplicative annf =>
    ElmVersion -> ImportInfo [UppercaseIdentifier]
    -> C2 before after (ASTNS annf [UppercaseIdentifier] 'ExpressionNK)
    -> C2 before after (ASTNS annf [UppercaseIdentifier] 'ExpressionNK)
    -> (SyntaxContext, Block)
formatRange_0_18 elmVersion importInfo left right =
    case (left, right) of
        (C (preLeft, []) left', C (preRight, []) right') ->
            App
                (I.Fix $ Identity $ VarExpr $ VarRef [UppercaseIdentifier "List"] $ LowercaseIdentifier "range")
                [ C preLeft $ I.convert (pure . extract) left'
                , C preRight $ I.convert (pure . extract) right'
                ]
                (FAJoinFirst JoinAll)
                |> (I.Fix . pure)
                |> formatExpression elmVersion importInfo

        _ ->
            App
                (I.Fix $ Identity $ VarExpr $ VarRef [UppercaseIdentifier "List"] $ LowercaseIdentifier "range")
                [ C [] $ I.Fix $ pure $ Parens $ fmap (I.convert (pure . extract)) left
                , C [] $ I.Fix $ pure $ Parens $ fmap (I.convert (pure . extract)) right
                ]
                (FAJoinFirst JoinAll)
                |> (I.Fix . pure)
                |> formatExpression elmVersion importInfo


formatUnit :: Char -> Char -> Comments -> Block
formatUnit left right comments =
  case (left, comments) of
    (_, []) ->
      line $ punc [left, right]

    ('{', (LineComment _):_) ->
      surround left right $ prefix space $ stack1 $ map formatComment comments

    _ ->
      surround left right $
        case allSingles' $ map formatComment comments of
          Right comments' ->
            line $ row $ List.intersperse space comments'

          Left comments' ->
            stack1 comments'


formatComments :: Comments -> Maybe Block
formatComments comments =
    case fmap formatComment comments of
        [] ->
            Nothing

        (first:rest) ->
            Just $ spaceSepOrStack (first :| rest)


formatCommented_ :: Bool -> C2 before after Block -> Block
formatCommented_ forceMultiline (C (pre, post) inner) =
    spaceSepOrStackForce forceMultiline $ NonEmpty.fromList $ mconcat
        [ Maybe.maybeToList $ formatComments pre
        , [inner]
        , Maybe.maybeToList $ formatComments post
        ]


formatCommented :: C2 before after Block -> Block
formatCommented =
  formatCommented_ False


formatPreCommented :: C1 before Block -> Block
formatPreCommented (C pre inner) =
    formatCommented' pre inner


formatCommented' :: Comments -> Block -> Block
formatCommented' pre inner =
    formatCommented (C (pre, []) inner)


formatTailCommented :: C1 after Block -> Block
formatTailCommented (C post inner) =
  formatCommented (C ([], post) inner)


formatC2Eol :: C2Eol before after Block -> Block
formatC2Eol (C (pre, post, eol) a) =
    let
        forceMultilinePost =
          case listToMaybe post of
              Just (LineComment _) -> True
              _ -> False
    in
    formatCommented_ forceMultilinePost $ C (pre, post) $ formatEolCommented $ C eol a


formatEolCommented :: C0Eol Block -> Block
formatEolCommented (C post inner) =
  case post of
    Nothing -> inner
    Just eol ->
      spaceSepOrStack
          [ inner
          , formatComment $ LineComment eol
          ]


formatCommentedStack :: C2 before after Block -> Block
formatCommentedStack (C (pre, post) inner) =
  stack1 $
    map formatComment pre
      ++ [ inner ]
      ++ map formatComment post


formatPreCommentedStack :: C1 before Block -> Block
formatPreCommentedStack (C pre inner) =
  formatCommentedStack (C (pre, []) inner)


formatKeywordCommented :: String -> C2 beforeKeyword afterKeyword Block -> Block
formatKeywordCommented word (C (pre, post) value) =
    spaceSepOrIndented
        [ formatCommented $ fmap (line . keyword) (C (pre, post) word)
        , value
        ]


formatOpenCommentedList :: OpenCommentedList Block -> [Block]
formatOpenCommentedList (OpenCommentedList rest (C (preLst, eol) lst)) =
    fmap formatC2Eol rest
        ++ [formatC2Eol $ C (preLst, [], eol) lst]


formatComment :: Comment -> Block
formatComment comment =
    case comment of
        BlockComment c ->
            case c of
                [] ->
                    line $ punc "{- -}"
                [l] ->
                    line $ row
                        [ punc "{-"
                        , space
                        , literal l
                        , space
                        , punc "-}"
                        ]
                ls ->
                    stack1
                        [ prefix
                            (row [ punc "{-", space ])
                            (stack1 $ map (line . literal) ls)
                        , line $ punc "-}"
                        ]

        LineComment c ->
            Block.mustBreak $ row [ punc "--", literal c ]

        CommentTrickOpener ->
            Block.mustBreak $ punc "{--}"

        CommentTrickCloser ->
            Block.mustBreak $ punc "--}"

        CommentTrickBlock c ->
            Block.mustBreak $ row [ punc "{--", literal c, punc "-}" ]


formatLiteral :: ElmVersion -> LiteralValue -> Block
formatLiteral elmVersion lit =
    case lit of
        IntNum i DecimalInt ->
            line $ literal $ show i
        IntNum i HexadecimalInt ->
            line $ literal $
              if i < -0xFFFFFFFF then
                printf "-0x%016X" (-i)
              else if i < -0xFFFF then
                printf "-0x%08X" (-i)
              else if i < -0xFF then
                printf "-0x%04X" (-i)
              else if i < 0 then
                printf "-0x%02X" (-i)
              else if i <= 0xFF then
                printf "0x%02X" i
              else if i <= 0xFFFF then
                printf "0x%04X" i
              else if i <= 0xFFFFFFFF then
                printf "0x%08X" i
              else
                printf "0x%016X" i
        FloatNum f DecimalFloat ->
            line $ literal $ printf "%f" f
        FloatNum f ExponentFloat ->
            line $ literal $ printf "%e" f
        Chr c ->
            formatString elmVersion SChar [c]
        Str s multi ->
            formatString elmVersion (SString multi) s
        Boolean b ->
            line $ literal $ show b


data StringStyle
    = SChar
    | SString StringRepresentation
    deriving (Eq)


formatString :: ElmVersion -> StringStyle -> String -> Block
formatString elmVersion style s =
  case style of
      SChar ->
        stringBox "\'" id
      SString SingleQuotedString ->
        stringBox "\"" id
      SString TripleQuotedString ->
        stringBox "\"\"\"" escapeMultiQuote
  where
    stringBox quotes escaper =
      line $ row
          [ punc quotes
          , literal $ escaper $ concatMap fix s
          , punc quotes
          ]

    fix c =
        if (style == SString TripleQuotedString) && c == '\n' then
            [c]
        else if c == '\n' then
            "\\n"
        else if c == '\t' then
            "\\t"
        else if c == '\\' then
            "\\\\"
        else if (style == SString SingleQuotedString) && c == '\"' then
            "\\\""
        else if (style == SChar) && c == '\'' then
            "\\\'"
        else if not $ Char.isPrint c then
            hex c
        else if c == ' ' then
            [c]
        else if ElmVersion.style_0_19_stringEscape elmVersion == False && c == '\xA0' then
            [c] -- Workaround for https://github.com/elm-lang/elm-compiler/issues/1279
        else if Char.isSpace c then
            hex c
        else
            [c]

    hex char =
      case ElmVersion.style_0_19_stringEscape elmVersion of
          True ->
              "\\u{" ++ printf "%04X" (Char.ord char) ++ "}"
          False ->
              "\\x" ++ printf fmt (Char.ord char)
      where
        fmt =
          if Char.ord char <= 0xFF then
            "%02X"
          else
            "%04X"

    escapeMultiQuote =
        let
            step okay quotes remaining =
                case remaining of
                    [] ->
                        reverse $ concat (replicate quotes "\"\\") ++ okay

                    next : rest ->
                        if next == '"' then
                            step okay (quotes + 1) rest
                        else if quotes >= 3 then
                            step (next : concat (replicate quotes "\"\\") ++ okay) 0 rest
                        else if quotes > 0 then
                            step (next : replicate quotes '"' ++ okay) 0 rest
                        else
                            step (next : okay) 0 rest
        in
            step "" 0



data TypeParensRequired
    = {- 0 -} NotRequired
    | {- 1 -} ForLambda
    | {- 2 -} ForCtor
    deriving (Eq, Ord)


data TypeParensInner
    = NotNeeded
    | ForFunctionType
    | ForTypeConstruction


typeParens :: TypeParensRequired -> (TypeParensInner, Block) -> Block
typeParens outer (inner, box) =
    if typeParensNeeded outer inner then parens box else box


typeParensNeeded :: TypeParensRequired -> TypeParensInner -> Bool
typeParensNeeded outer = \case
    NotNeeded -> False
    ForTypeConstruction -> outer >= ForCtor
    ForFunctionType -> outer >= ForLambda


commaSpace :: Line
commaSpace =
    row
        [ punc ","
        , space
        ]


formatTypeConstructor :: ElmVersion -> TypeConstructor ([UppercaseIdentifier], UppercaseIdentifier) -> Block
formatTypeConstructor elmVersion ctor =
    case ctor of
        NamedConstructor (namespace, name) ->
            line $ formatQualifiedUppercaseIdentifier elmVersion (namespace ++ [name])

        TupleConstructor n ->
            line $ keyword $ "(" ++ List.replicate (n-1) ',' ++ ")"


formatType ::
    Coapplicative annf =>
    ElmVersion -> ASTNS annf [UppercaseIdentifier] 'TypeNK -> (TypeParensInner, Block)
formatType elmVersion atype =
    case extract $ I.unFix atype of
        UnitType comments ->
          (,) NotNeeded $
          formatUnit '(' ')' comments

        FunctionType first rest (ForceMultiline forceMultiline) ->
            let
                formatRight (C (preOp, postOp, eol) term) =
                    spaceSepOrStackForce False $ NonEmpty.fromList $ mconcat
                        [ Maybe.maybeToList $ formatComments preOp
                        , [ prefixOrIndented
                              (punc "->")
                              (formatC2Eol $
                                  (fmap $ typeParens ForLambda . formatType elmVersion)
                                  (C (postOp, [], eol) term)
                              )
                          ]
                        ]
            in
                (,) ForFunctionType $
                spaceSepOrStackForce forceMultiline $
                    formatEolCommented (typeParens ForLambda . formatType elmVersion <$> first)
                    :| (formatRight <$> toCommentedList rest)

        TypeVariable var ->
            (,) NotNeeded $
            line $ identifier $ formatVarName elmVersion var

        TypeConstruction ctor args forceMultiline ->
            let
                join =
                    case forceMultiline of
                        ForceMultiline True -> FASplitFirst
                        ForceMultiline False -> FAJoinFirst JoinAll
            in
            (,) (if null args then NotNeeded else ForTypeConstruction) $
            ElmStructure.application
                join
                (formatTypeConstructor elmVersion ctor)
                (fmap (formatPreCommented . fmap (typeParens ForCtor . formatType elmVersion)) args)

        TypeParens type' ->
          (,) NotNeeded $
          parens $ formatCommented $ fmap (typeParens NotRequired . formatType elmVersion) type'

        TupleType types (ForceMultiline forceMultiline) ->
            (,) NotNeeded $
            ElmStructure.group True "(" "," ")" forceMultiline (fmap (formatC2Eol . fmap (typeParens NotRequired . formatType elmVersion)) types)

        RecordType base fields trailing multiline ->
            (,) NotNeeded $
            formatRecordLike
                (fmap (line . formatLowercaseIdentifier elmVersion []) <$> base)
                (fmap (formatPair ":" . mapPair (formatLowercaseIdentifier elmVersion []) (typeParens NotRequired . formatType elmVersion)) fields)
                trailing multiline


formatVar :: ElmVersion -> Ref [UppercaseIdentifier] -> Line
formatVar elmVersion var =
    case var of
        VarRef namespace name ->
            formatLowercaseIdentifier elmVersion namespace name

        TagRef namespace name ->
            case namespace of
                [] -> identifier $ formatVarName'' elmVersion name
                _ ->
                    row
                        [ formatQualifiedUppercaseIdentifier elmVersion namespace
                        , punc "."
                        , identifier $ formatVarName'' elmVersion name
                        ]

        OpRef name ->
            formatSymbolIdentifierInParens name


formatSymbolIdentifierInParens :: SymbolIdentifier -> Line
formatSymbolIdentifierInParens (SymbolIdentifier name) =
    identifier $ "(" ++ name ++ ")"


formatInfixVar :: ElmVersion -> Ref [UppercaseIdentifier] -> Line
formatInfixVar elmVersion var =
    case var of
        VarRef _ _ ->
            row [ punc "`"
                , formatVar elmVersion var
                , punc "`"
                ]
        TagRef _ _ ->
            row [ punc "`"
                , formatVar elmVersion var
                , punc "`"
                ]
        OpRef (SymbolIdentifier name) ->
            identifier name


formatLowercaseIdentifier :: ElmVersion -> [UppercaseIdentifier] -> LowercaseIdentifier -> Line
formatLowercaseIdentifier elmVersion namespace (LowercaseIdentifier name) =
    case (elmVersion, namespace, name) of
        (_, [], _) -> identifier $ formatVarName' elmVersion name
        _ ->
            row
                [ formatQualifiedUppercaseIdentifier elmVersion namespace
                , punc "."
                , identifier $ formatVarName' elmVersion name
                ]


formatUppercaseIdentifier :: ElmVersion -> UppercaseIdentifier -> Line
formatUppercaseIdentifier elmVersion (UppercaseIdentifier name) =
    identifier $ formatVarName' elmVersion name


formatQualifiedUppercaseIdentifier :: ElmVersion -> [UppercaseIdentifier] -> Line
formatQualifiedUppercaseIdentifier elmVersion names =
  identifier $ List.intercalate "." $
      map (\(UppercaseIdentifier name) -> formatVarName' elmVersion name) names


formatVarName :: ElmVersion -> LowercaseIdentifier -> String
formatVarName elmVersion (LowercaseIdentifier name) =
    formatVarName' elmVersion name


formatVarName' :: ElmVersion -> String -> String
formatVarName' elmVersion name =
    case elmVersion of
        Elm_0_16 -> name
        Elm_0_17 -> name
        _ -> map (\x -> if x == '\'' then '_' else x) name


formatVarName'' :: ElmVersion -> UppercaseIdentifier -> String
formatVarName'' elmVersion (UppercaseIdentifier name) =
    formatVarName' elmVersion name

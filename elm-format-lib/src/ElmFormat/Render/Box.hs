{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ElmFormat.Render.Box where

import Elm.Utils ((|>))
import Box ( Line, identifier, punc, space, render )
import ElmVersion (ElmVersion(..))

import AST.V0_16
import qualified AST.Module
import AST.Structure
import qualified AST.Listing
import qualified Cheapskate.Types as Markdown
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import Data.Coapplicative
import qualified Data.Foldable as Foldable
import Data.Functor.Identity (Identity(..))
import qualified Data.Indexed as I
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
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
import qualified Reporting.Result as Result
import Text.Printf (printf)
import ElmFormat.Render.ElmStructure (Elm, parens, keyword)
import qualified Data.Fix as Fix
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Either.Extra


pleaseReport :: String -> String -> a
pleaseReport what details =
    -- TODO: include version in the message
    error $ "<elm-format: " ++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >"


formatBinary :: Bool -> Elm -> [ ( Bool, Comments, Elm, Elm ) ] -> Elm
formatBinary multiline left ops =
    case ops of
        [] ->
            left

        ( isLeftPipe, comments, op, next ) : rest ->
            if isLeftPipe then
                ElmStructure.forceableSpaceSepOrIndented multiline
                    (ElmStructure.spaceSepOrStack left $
                        maybeToList (formatComments comments) ++ [op]
                    )
                    [formatBinary multiline next rest]
            else
                formatBinary
                    multiline
                    (ElmStructure.forceableSpaceSepOrIndented multiline left [formatCommented' comments $ ElmStructure.spaceSepOrPrefix op next])
                    rest


splitWhere :: (a -> Bool) -> [a] -> [[a]]
splitWhere predicate list =
    let
        merge acc =
            ReversedList.push (ReversedList.toList acc)

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
            case foldl stepChildren (ReversedList.empty, seen) next |> (\(a,b) -> (ReversedList.toList a, b)) of
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
            Set.fromList $ varName <$> concat listedInDocs

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
        then ( pure <$> remainingFromExposing, commentsFromReorderedVars )
        else ( listedInDocs ++ [remainingFromExposing | not (List.null remainingFromExposing)], commentsFromReorderedVars )


formatModuleHeader :: Coapplicative annf => ElmVersion -> Bool -> AST.Module.Module [UppercaseIdentifier] (I.Fix2 annf (ASTNS [UppercaseIdentifier]) 'TopLevelNK) -> (Maybe Elm, Maybe Elm, (Maybe Elm, List Elm))
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
                  fmap (refName . textToRef) <$> vars
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
              |> (extract . I.unFix2)
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
          Set.unions
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

      extractVarName :: Coapplicative annf => TopLevelStructure (I.Fix2 annf (ASTNS ns) 'TopLevelDeclarationNK) -> [AST.Listing.Value]
      extractVarName decl =
          case extract . I.unFix2 <$> decl of
              DocComment _ -> []
              BodyComment _ -> []
              Entry (PortAnnotation (C _ (LowercaseIdentifier name)) _ _) -> [ AST.Listing.Value (LowercaseIdentifier name) ]
              Entry (CommonDeclaration def) ->
                case extract $ I.unFix2 def of
                    Definition pat _ _ _ ->
                        case extract $ I.unFix2 pat of
                            VarPattern (LowercaseIdentifier name) -> [ AST.Listing.Value (LowercaseIdentifier name) ]
                            RecordPattern fields -> AST.Listing.Value . extract <$> fields
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
    ( formatModuleLine' <$> maybeHeader
    , docs
    , imports
    )


formatImports :: ElmVersion -> AST.Module.Module [UppercaseIdentifier] decl -> (Maybe Elm, [Elm])
formatImports elmVersion modu =
    let
        (C comments imports) =
            AST.Module.imports modu
    in
    ( formatComments comments
    , imports
        |> Map.assocs
        |> fmap (\(name, C pre method) -> formatImport elmVersion (C pre name, method))
    )


formatModuleLine_0_16 :: AST.Module.Header -> Elm
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

    whereClause =
        formatCommented (C whereComments $ keyword "where")
  in
    ElmStructure.spaceSepOrIndented
        (keyword "module")
        [ formatCommented $ formatUppercaseIdentifier' . fmap (fixUppercaseIdentifier elmVersion) <$> AST.Module.name header
        , formatExports
        , whereClause
        ]


formatModuleLine ::
    ElmVersion
    -> ([[C2 before after AST.Listing.Value]], Comments)
    -> AST.Module.SourceTag
    -> C2 before after [UppercaseIdentifier]
    -> Maybe (C2 before after AST.Module.SourceSettings)
    -> Comments
    -> Comments
    -> Elm
formatModuleLine elmVersion (varsToExpose, extraComments) srcTag name moduleSettings preExposing postExposing =
    let
        tag =
            case srcTag of
                AST.Module.Normal ->
                    keyword "module"

                AST.Module.Port comments ->
                    ElmStructure.spaceSepOrIndented
                        (formatTailCommented (C comments $ keyword "port"))
                        [ keyword "module" ]

                AST.Module.Effect comments ->
                    ElmStructure.spaceSepOrIndented
                        (formatTailCommented (C comments $ keyword "effect"))
                        [ keyword "module" ]

        exports =
            case varsToExpose of
                [] -> keyword "(..)"
                [oneGroup] ->
                    ElmStructure.group' False "(" "," (formatComments extraComments) ")" False $
                    formatCommented . fmap (formatVarValue elmVersion) <$> oneGroup
                _ ->
                    ElmStructure.group' False "(" "," (formatComments extraComments) ")" True $
                    formatCommented . fmap (ElmStructure.group False "" "," "" False . fmap (formatVarValue elmVersion)) . sequenceA <$> varsToExpose

        formatSetting (k, v) =
            formatRecordPair elmVersion "=" (k, formatUppercaseIdentifier [] . fixUppercaseIdentifier elmVersion <$> v, False)

        formatSettings settings =
            ElmStructure.group True "{" "," "}" False $
            formatSetting <$> settings

        whereClause =
            maybeToList $
            formatKeywordCommented "where" . fmap formatSettings <$> moduleSettings

        nameClause =
            ElmStructure.spaceSepOrIndented tag
                [formatCommented $ formatUppercaseIdentifier' . fmap (fixUppercaseIdentifier elmVersion) <$> name]
  in
  ElmStructure.spaceSepOrIndented
      (ElmStructure.spaceSepOrIndented
          nameClause
          (whereClause ++ [formatCommented (C (preExposing, postExposing) $ keyword "exposing")])
      )
      [ exports ]


formatModule :: Coapplicative annf => ElmVersion -> Bool -> Int -> AST.Module.Module [UppercaseIdentifier] (I.Fix2 annf (ASTNS [UppercaseIdentifier]) 'TopLevelNK) -> Elm
formatModule elmVersion addDefaultHeader spacing modu =
    let
        spaceBeforeBody =
            case extract $ I.unFix2 $ AST.Module.body modu of
                TopLevel [] -> 0
                TopLevel (BodyComment _ : _) -> spacing + 1
                TopLevel _ -> spacing

        decls =
          case extract $ I.unFix2 $ AST.Module.body modu of
              TopLevel decls -> decls
    in
    ElmStructure.module'
        (formatComment <$> AST.Module.initialComments modu)
        (formatModuleHeader elmVersion addDefaultHeader modu)
        spaceBeforeBody
        (formatModuleBody spacing elmVersion (ImportInfo.fromModule mempty modu) decls)


formatModuleBody :: forall annf. Coapplicative annf => Int -> ElmVersion -> ImportInfo [UppercaseIdentifier] -> [TopLevelStructure (I.Fix2 annf (ASTNS [UppercaseIdentifier]) 'TopLevelDeclarationNK)] -> Maybe Elm
formatModuleBody linesBetween elmVersion importInfo body =
    let
        entryType :: I.Fix2 annf (ASTNS ns) 'TopLevelDeclarationNK -> BodyEntryType
        entryType adecl =
            case extract $ I.unFix2 adecl of
                CommonDeclaration def ->
                    case extract $ I.unFix2 def of
                        Definition pat _ _ _ ->
                            case extract $ I.unFix2 pat of
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
        fmap (\b -> (entryType b, formatDeclaration elmVersion importInfo b)) <$> body


data BodyEntryType
    = BodyNamed (Ref ())
    | BodyUnnamed
    | BodyFixity


formatTopLevelBody ::
    Int
    -> ElmVersion
    -> ImportInfo [UppercaseIdentifier]
    -> [TopLevelStructure (BodyEntryType, Elm)]
    -> Maybe Elm
formatTopLevelBody linesBetween elmVersion importInfo body =
    case body of
        [] -> Nothing
        first:rest ->
            Just $
            stackWithSpacing
                (\a b -> topLevelSpacer linesBetween (fst <$> a) (fst <$> b))
                (formatTopLevelStructure elmVersion importInfo . fmap snd)
                first rest


stackWithSpacing :: (a -> a -> Int) -> (a -> Elm) -> a -> [a] -> Elm
stackWithSpacing getSpacing format first rest =
    let
        spacing =
            zipWith getSpacing (first:rest) rest
    in
    ElmStructure.stackWithVariableSpacing
        (format first)
        (zip spacing (format <$> rest))


{-| How many blank lines should go between two given top-level declarations.
-}
topLevelSpacer :: Int -> TopLevelStructure BodyEntryType -> TopLevelStructure BodyEntryType -> Int
topLevelSpacer linesBetween a b =
    case (declarationType a, declarationType b) of
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


data ElmCodeBlock annf ns
    = DeclarationsCode [TopLevelStructure (I.Fix2 annf (ASTNS ns) 'TopLevelDeclarationNK)]
    | ExpressionsCode [TopLevelStructure (C0Eol (I.Fix2 annf (ASTNS ns) 'ExpressionNK))]
    | ModuleCode (AST.Module.Module ns (I.Fix2 annf (ASTNS ns) 'TopLevelNK))

convertElmCodeBlock :: Functor ann => (forall x. ann x -> ann' x) -> ElmCodeBlock ann ns -> ElmCodeBlock ann' ns
convertElmCodeBlock f = \case
    DeclarationsCode decls -> DeclarationsCode (fmap (I.convert f) <$> decls)
    ExpressionsCode exprs -> ExpressionsCode (fmap (fmap $ I.convert f) <$> exprs)
    ModuleCode mod -> ModuleCode (I.convert f <$> mod)


-- TODO: there must be an existing haskell function that does this, right?
firstOf :: [a -> Maybe b] -> a -> Maybe b
firstOf options value =
    case options of
        [] -> Nothing
        (next:rest) ->
            case next value of
                Just result -> Just result
                Nothing -> firstOf rest value


formatDocComment :: ElmVersion -> ImportInfo [UppercaseIdentifier] -> Markdown.Blocks -> Elm
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
                        |> (Text.unpack . Box.render . Fix.cata ElmStructure.render)

                DeclarationsCode declarations ->
                    formatModuleBody 1 elmVersion importInfo declarations
                        |> fmap (Text.unpack . Box.render . Fix.cata ElmStructure.render)
                        |> fromMaybe ""

                ExpressionsCode expressions ->
                    expressions
                        |> fmap (fmap $ fmap $ I.convert (Identity . extract))
                        |> fmap (fmap $ formatEolCommented . fmap (syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo))
                        |> fmap (fmap $ (,) BodyUnnamed)
                        |> formatTopLevelBody 1 elmVersion importInfo
                        |> fmap (Text.unpack . Box.render . Fix.cata ElmStructure.render)
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
    ElmStructure.docComment "{-|" "-}"
        (Text.lines $ Text.pack content)


formatImport :: ElmVersion -> AST.Module.UserImport -> Elm
formatImport elmVersion (name@(C _ rawName), method) =
    let
        name' =
            formatPreCommented $ formatUppercaseIdentifier' . fmap (fixUppercaseIdentifier elmVersion) <$> name

        requestedAs =
            case AST.Module.alias method of
                Just (C _ aliasName) | [aliasName] == rawName -> Nothing
                other -> other

        as =
            requestedAs
                |> fmap (formatImportClause "as" . fmap (Just . formatUppercaseIdentifier [] . fixUppercaseIdentifier elmVersion))
                |> Monad.join

        exposing =
          formatImportClause "exposing"
            (formatListing (formatDetailedListing elmVersion) <$> AST.Module.exposedVars method)

        formatImportClause :: Text -> C2 beforeKeyword afterKeyword (Maybe Elm) -> Maybe Elm
        formatImportClause keyw = \case
            C ([], []) Nothing ->
                Nothing

            C (preKeyword, postKeyword) (Just listing') ->
                Just $ ElmStructure.spaceSepOrIndented
                    (formatPreCommented (C preKeyword $ keyword keyw))
                    [ formatPreCommented (C postKeyword listing') ]

            _ ->
                pleaseReport "UNEXPECTED IMPORT" "import clause comments with no clause"
    in
    ElmStructure.import' name' as exposing


formatListing :: (a -> [Elm]) -> AST.Listing.Listing a -> Maybe Elm
formatListing format listing =
    case listing of
        AST.Listing.ClosedListing ->
            Nothing

        AST.Listing.OpenListing (C comments ()) ->
            Just $ parens $ formatCommented $ C comments $ keyword ".."

        AST.Listing.ExplicitListing vars multiline ->
            case format vars of
                [] -> Nothing
                vars' -> Just $ ElmStructure.group False "(" "," ")" multiline vars'


formatDetailedListing :: ElmVersion -> AST.Module.DetailedListing -> [Elm]
formatDetailedListing elmVersion listing =
    concat
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


formatCommentedMap :: (k -> v -> a) -> (a -> Elm) -> AST.Listing.CommentedMap k v -> [Elm]
formatCommentedMap construct format values =
    let
        format' (k, C c v)
            = formatCommented $ C c (format $ construct k v)
    in
    format' <$> Map.assocs values


formatVarValue :: ElmVersion -> AST.Listing.Value -> Elm
formatVarValue elmVersion aval =
    case aval of
        AST.Listing.Value val ->
            formatLowercaseIdentifier [] $ fixLowercaseIdentifier elmVersion val

        AST.Listing.OpValue op ->
            formatSymbolIdentifierInParens op

        AST.Listing.Union name listing ->
            let
                listing' =
                    formatListing
                        (formatCommentedMap
                            (\name_ () -> name_)
                            (formatUppercaseIdentifier [] . fixUppercaseIdentifier elmVersion)
                        )
                        listing
            in
            ElmStructure.unionListing
                (formatTailCommented $ formatUppercaseIdentifier [] . fixUppercaseIdentifier elmVersion <$> name)
                ([] /= (\(C c _) -> c) name)
                listing'


formatTopLevelStructure :: ElmVersion -> ImportInfo [UppercaseIdentifier] -> TopLevelStructure Elm -> Elm
formatTopLevelStructure elmVersion importInfo topLevelStructure =
    case topLevelStructure of
        DocComment docs ->
            formatDocComment elmVersion importInfo docs

        BodyComment c ->
            formatComment c

        Entry entry ->
            entry


data FormatResult (nk :: NodeKind) where
    FormattedExpression :: SyntaxContext -> Elm -> FormatResult 'ExpressionNK
    FormattedPattern :: SyntaxContext -> Elm -> FormatResult 'PatternNK


formatAst :: Coapplicative annf => ElmVersion -> ImportInfo [UppercaseIdentifier] -> I.Fix2 annf (ASTNS [UppercaseIdentifier]) nk -> FormatResult nk
formatAst elmVersion importInfo =
    I.fold2 (formatAstNode elmVersion importInfo . extract)


formatCommonDeclaration ::
    Coapplicative annf =>
    ElmVersion -> ImportInfo [UppercaseIdentifier] -> I.Fix2 annf (ASTNS [UppercaseIdentifier]) 'CommonDeclarationNK -> Elm
formatCommonDeclaration elmVersion importInfo decl =
    case extract $ I.unFix2 $ I.convert (Identity . extract) decl of
        Definition name args comments expr ->
            formatDefinition elmVersion importInfo name args comments expr

        TypeAnnotation name typ ->
            formatTypeAnnotation elmVersion name typ


formatDeclaration ::
    Coapplicative annf =>
    ElmVersion -> ImportInfo [UppercaseIdentifier] -> I.Fix2 annf (ASTNS [UppercaseIdentifier]) 'TopLevelDeclarationNK -> Elm
formatDeclaration elmVersion importInfo decl =
    case extract $ I.unFix2 $ I.convert (Identity . extract) decl of
        CommonDeclaration def ->
            formatCommonDeclaration elmVersion importInfo def

        Datatype nameWithArgs tags ->
            let
                ctor (NameWithArgs tag args') =
                    ElmStructure.spaceSepOrIndented
                        (formatUppercaseIdentifier [] $ fixUppercaseIdentifier elmVersion tag)
                        (formatPreCommented . fmap (typeParens ForCtor . formatType elmVersion) <$> args')

                leftSide =
                    ElmStructure.spaceSepOrIndented
                        (keyword "type")
                        [ formatCommented $ formatNameWithArgs elmVersion <$> nameWithArgs
                        ]

                variants =
                    case formatOpenCommentedList $ ctor <$> tags of
                        [] -> pleaseReport "UNEXPECTED CUSTOM TYPE DECLARATION" "No variants"
                        first:rest ->
                            ElmStructure.spaceSepOrPrefix (keyword "=") first
                            : (ElmStructure.spaceSepOrPrefix (keyword "|") <$> rest)
            in
            ElmStructure.stackIndent leftSide variants

        TypeAlias preAlias nameWithArgs typ ->
            ElmStructure.definition "=" True
            (keyword "type")
            [ formatPreCommented (C preAlias $ keyword "alias")
            , formatCommented $ formatNameWithArgs elmVersion <$> nameWithArgs
            ]
            (formatPreCommentedStack $ typeParens NotRequired . formatType elmVersion <$> typ)

        PortAnnotation name typeComments typ ->
            ElmStructure.definition ":" False
            (keyword "port")
            [ formatCommented $ formatLowercaseIdentifier [] . fixLowercaseIdentifier elmVersion <$> name ]
            (formatCommented' typeComments $ typeParens NotRequired $ formatType elmVersion typ)

        PortDefinition_until_0_16 name bodyComments expr ->
            ElmStructure.definition "=" True
            (keyword "port")
            [formatCommented $ formatLowercaseIdentifier [] . fixLowercaseIdentifier elmVersion <$> name]
            (formatCommented' bodyComments $ syntaxParens SyntaxSeparated $ formatExpression elmVersion importInfo expr)

        Fixity_until_0_18 assoc precedenceComments precedence nameComments name ->
            ElmStructure.spaceSepOrIndented
                (formatInfixAssociativity_0_18 assoc)
                [ formatCommented' precedenceComments $ formatInfixPrecedence precedence
                , formatCommented' nameComments $ formatInfixVar elmVersion name
                ]

        Fixity assoc precedence name value ->
            ElmStructure.spaceSepOrIndented
                (keyword "infix")
                [ formatPreCommented $ formatInfixAssociativity_0_19 <$> assoc
                , formatPreCommented $ formatInfixPrecedence <$> precedence
                , formatCommented $ formatSymbolIdentifierInParens <$> name
                , keyword "="
                , formatPreCommented $ formatLowercaseIdentifier [] . fixLowercaseIdentifier elmVersion <$> value
                ]


formatInfixAssociativity_0_18 :: Assoc -> Elm
formatInfixAssociativity_0_18 assoc =
    keyword $
    case assoc of
        L -> "infixl"
        R -> "infixr"
        N -> "infix"


formatInfixAssociativity_0_19 :: Assoc -> Elm
formatInfixAssociativity_0_19 a =
    keyword $
    case a of
        L -> "left "
        R -> "right"
        N -> "non  "


formatInfixPrecedence :: Int -> Elm
formatInfixPrecedence =
    ElmStructure.literal . Text.pack . show


formatNameWithArgs :: ElmVersion -> NameWithArgs UppercaseIdentifier LowercaseIdentifier -> Elm
formatNameWithArgs elmVersion (NameWithArgs name args) =
    ElmStructure.spaceSepOrIndented
        (formatUppercaseIdentifier [] $ fixUppercaseIdentifier elmVersion name)
        (formatPreCommented . fmap (formatLowercaseIdentifier [] . fixLowercaseIdentifier elmVersion) <$> args)


formatDefinition ::
    ElmVersion
    -> ImportInfo [UppercaseIdentifier]
    -> I.Fix2 Identity (ASTNS [UppercaseIdentifier]) 'PatternNK
    -> [C1 before (I.Fix2 Identity (ASTNS [UppercaseIdentifier]) 'PatternNK)]
    -> Comments
    -> I.Fix2 Identity (ASTNS [UppercaseIdentifier]) 'ExpressionNK
    -> Elm
formatDefinition elmVersion importInfo name args comments expr =
    let
        body =
            ElmStructure.stack1 $ mconcat
                [ formatComment <$> comments
                , [ syntaxParens SyntaxSeparated $ formatExpression elmVersion importInfo expr ]
                ]
    in
    ElmStructure.definition "=" True
      (syntaxParens SpaceSeparated $ formatAst elmVersion importInfo name)
      (map (\(C x y) -> formatCommented' x $ syntaxParens SpaceSeparated $ formatAst elmVersion importInfo y) args)
      body


formatTypeAnnotation ::
    Coapplicative annf =>
    ElmVersion -> C1 after (Ref ()) -> C1 before (I.Fix2 annf (ASTNS [UppercaseIdentifier]) 'TypeNK) -> Elm
formatTypeAnnotation elmVersion name typ =
  ElmStructure.definition ":" False
    (formatTailCommented $ formatVar elmVersion . fmap (\() -> []) <$> name)
    []
    (formatPreCommented $ typeParens NotRequired . formatType elmVersion <$> typ)


formatAstNode ::
    CtorRef p ~ ([UppercaseIdentifier], UppercaseIdentifier) =>
    ElmVersion -> ImportInfo [UppercaseIdentifier] -> AST p FormatResult nk -> FormatResult nk
formatAstNode elmVersion importInfo =
    \case
        Anything ->
            FormattedPattern SyntaxSeparated $ keyword "_"

        UnitPattern comments ->
            FormattedPattern SyntaxSeparated $ formatUnit '(' ')' comments

        LiteralPattern lit ->
            FormattedPattern SyntaxSeparated $ formatLiteral elmVersion lit

        VarPattern var ->
            FormattedPattern SyntaxSeparated $ formatLowercaseIdentifier [] $ fixLowercaseIdentifier elmVersion var

        OpPattern op ->
            FormattedPattern SyntaxSeparated $
            formatSymbolIdentifierInParens op

        ConsPattern first rest ->
            let
                formatRight (C (preOp, postOp, eol) term) =
                    ( False
                    , preOp
                    , keyword "::"
                    , formatC2Eol $ C (postOp, [], eol) $ syntaxParens SpaceSeparated term
                    )
            in
                FormattedPattern SpaceSeparated $
                formatBinary False
                    (formatEolCommented $ syntaxParens SpaceSeparated <$> first)
                    (formatRight <$> toCommentedList rest)

        DataPattern (ns, tag) [] ->
            formatUppercaseIdentifier ns (fixUppercaseIdentifier elmVersion tag)
                |>
                    case (elmVersion, ns) of
                        (Elm_0_16, []) ->
                            FormattedPattern SyntaxSeparated
                        (Elm_0_16, _:_) ->
                            FormattedPattern SpaceSeparated
                        _ ->
                            FormattedPattern SyntaxSeparated

        DataPattern (ns, tag) (pat0:pats) ->
            FormattedPattern SpaceSeparated $
            ElmStructure.application
                (FAJoinFirst JoinAll)
                (formatUppercaseIdentifier ns $ fixUppercaseIdentifier elmVersion tag)
                (formatPreCommented . fmap (syntaxParens SpaceSeparated) <$> pat0:|pats)

        PatternParens pattern ->
            FormattedPattern SyntaxSeparated $
            parens $ formatCommented $ syntaxParens SyntaxSeparated <$> pattern

        TuplePattern patterns ->
            FormattedPattern SyntaxSeparated $
            ElmStructure.group True "(" "," ")" False $ formatCommented . fmap (syntaxParens SyntaxSeparated) <$> patterns

        EmptyListPattern comments ->
            FormattedPattern SyntaxSeparated $
            formatUnit '[' ']' comments

        ListPattern patterns ->
            FormattedPattern SyntaxSeparated $
            ElmStructure.group True "[" "," "]" False $ formatCommented . fmap (syntaxParens SyntaxSeparated) <$> patterns

        EmptyRecordPattern comments ->
            FormattedPattern SyntaxSeparated $
            formatUnit '{' '}' comments

        RecordPattern fields ->
            FormattedPattern SyntaxSeparated $
            ElmStructure.group True "{" "," "}" False $ formatCommented . fmap (formatLowercaseIdentifier [] . fixLowercaseIdentifier elmVersion) <$> fields

        Alias pattern name ->
            FormattedPattern SpaceSeparated $
            ElmStructure.spaceSepOrStack
                (formatTailCommented $ syntaxParens SpaceSeparated <$> pattern)
                [ ElmStructure.spaceSepOrIndented
                    (keyword "as")
                    [ formatPreCommented $ formatLowercaseIdentifier [] . fixLowercaseIdentifier elmVersion <$> name]
                ]


formatRecordPair :: ElmVersion -> Text -> (C2 before after LowercaseIdentifier, C2 before after Elm, Bool) -> Elm
formatRecordPair elmVersion delim (C (pre, postK) k, v, forceMultiline) =
    formatPreCommented $ C pre $
    ElmStructure.equalsPair delim forceMultiline
        (formatCommented $ formatLowercaseIdentifier [] . fixLowercaseIdentifier elmVersion <$> C ([], postK) k)
        (formatCommented v)


formatPair :: Text -> Pair Elm Elm -> Elm
formatPair delim (Pair a b (ForceMultiline forceMultiline)) =
    ElmStructure.equalsPair delim forceMultiline
        (formatTailCommented a)
        (formatPreCommented b)


negativeCasePatternWorkaround ::
    Coapplicative annf =>
    I.Fix2 annf (ASTNS [UppercaseIdentifier]) 'PatternNK -> Elm -> Elm
negativeCasePatternWorkaround pattern =
    case extract $ I.unFix2 pattern of
        LiteralPattern (IntNum i _) | i < 0 -> parens
        LiteralPattern (FloatNum f _) | f < 0 -> parens
        _ -> id


data SyntaxContext
    = SyntaxSeparated
    | InfixSeparated
    | SpaceSeparated
    | AmbiguousEnd


class UsesSyntaxParens a where
    type Context a
    parensNeeded :: Context a -> a -> Bool
    getBox :: a -> Elm

instance UsesSyntaxParens (FormatResult 'ExpressionNK) where
    type Context (FormatResult 'ExpressionNK) = SyntaxContext
    parensNeeded outer (FormattedExpression inner _) = needsParensInContext inner outer
    getBox (FormattedExpression _ box) = box

instance UsesSyntaxParens (FormatResult 'PatternNK) where
    type Context (FormatResult 'PatternNK) = SyntaxContext
    parensNeeded outer (FormattedPattern inner _) = needsParensInContext inner outer
    getBox (FormattedPattern _ box) = box

syntaxParens :: UsesSyntaxParens a => Context a -> a -> Elm
syntaxParens outer a =
    parensIf (parensNeeded outer a) (getBox a)
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
    -> I.Fix2 Identity (ASTNS [UppercaseIdentifier]) 'ExpressionNK
    -> FormatResult 'ExpressionNK
formatExpression elmVersion importInfo aexpr =
    case extract $ I.unFix2 aexpr of
        Literal lit ->
            FormattedExpression SyntaxSeparated $ formatLiteral elmVersion lit

        VarExpr v ->
            FormattedExpression SyntaxSeparated $ formatVar elmVersion v

        Range left right ->
            case elmVersion of
                Elm_0_16 -> FormattedExpression SyntaxSeparated $ formatRange_0_17 elmVersion importInfo left right
                Elm_0_17 -> FormattedExpression SyntaxSeparated $ formatRange_0_17 elmVersion importInfo left right
                Elm_0_18 -> formatRange_0_18 elmVersion importInfo left right
                Elm_0_19 -> formatRange_0_18 elmVersion importInfo left right

        ExplicitList exprs trailing multiline ->
            FormattedExpression SyntaxSeparated $
            formatSequenceAsGroup '[' ',' ']'
                multiline
                trailing
                (syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo <$> exprs)

        Binops left ops multiline ->
            FormattedExpression InfixSeparated $
            formatBinops elmVersion importInfo left ops multiline

        Lambda [] _ _ _ ->
            pleaseReport "UNEXPECTED LAMBDA" "no patterns"

        Lambda (pat1:pats) bodyComments expr multiline ->
            let
                patterns' =
                    ElmStructure.forceableSpaceSepOrStack1 False
                    (formatPreCommented . fmap (syntaxParens SpaceSeparated . formatAst elmVersion importInfo) <$> pat1:pats)
            in
            FormattedExpression AmbiguousEnd $
            ElmStructure.lambda "\\" "->" multiline
                patterns'
                (formatComments bodyComments)
                (syntaxParens SyntaxSeparated $ formatExpression elmVersion importInfo expr)

        Unary Negative e ->
            FormattedExpression SyntaxSeparated $
            ElmStructure.unary (keyword "-") $
            syntaxParens SpaceSeparated $ formatExpression elmVersion importInfo e -- TODO: This might need something stronger than SpaceSeparated?

        App left [] _ ->
            formatExpression elmVersion importInfo left

        App left (arg0:args) multiline ->
            FormattedExpression SpaceSeparated $
            ElmStructure.application
                multiline
                (syntaxParens InfixSeparated $ formatExpression elmVersion importInfo left)
                (formatPreCommentedExpression elmVersion importInfo SpaceSeparated <$> arg0:|args)

        If (IfClause cond body) elseifs (C elsComments els) ->
            let
                formatElseIf (C ifComments (IfClause cond' body')) =
                    ( formatComments ifComments
                    , formatCommentedExpression elmVersion importInfo cond'
                    , formatCommented_ True $ syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo <$> body'
                    )
            in
            FormattedExpression AmbiguousEnd $
            ElmStructure.ifElse "if" "then" "else"
                (formatCommentedExpression elmVersion importInfo cond)
                (formatCommented_ True $ syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo <$> body)
                (formatElseIf <$> elseifs)
                (formatCommented_ True $ syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo <$> C (elsComments, []) els)

        Let [] _ _ ->
            pleaseReport "UNEXPECTED LET EXPRESSION" "No declarations"

        Let (def1:defs) bodyComments expr ->
            let
                spacer :: AST p (I.Fix2 Identity (AST p)) 'LetDeclarationNK -> letDecl -> Int
                spacer first _ =
                    case first of
                        LetCommonDeclaration (I.Fix2 (Identity (Definition _ _ _ _))) -> 1
                        _ -> 0

                formatDefinition' def =
                  case def of
                    LetCommonDeclaration (I.Fix2 (Identity (Definition name args comments expr'))) ->
                      formatDefinition elmVersion importInfo name args comments expr'

                    LetCommonDeclaration (I.Fix2 (Identity (TypeAnnotation name typ))) ->
                      formatTypeAnnotation elmVersion name typ

                    LetComment comment ->
                        formatComment comment
            in
                FormattedExpression AmbiguousEnd $ -- TODO: not tested
                ElmStructure.letIn "let" "in"
                    (stackWithSpacing
                        (\a b -> spacer (extract $ I.unFix2 a) b)
                        (formatDefinition' . extract . I.unFix2)
                        def1 defs
                    )
                    (ElmStructure.stack1 $
                        fmap formatComment bodyComments
                            ++ [syntaxParens SyntaxSeparated $ formatExpression elmVersion importInfo expr]
                    )

        Case (subject,multiline) clauses ->
            FormattedExpression AmbiguousEnd $ -- TODO: not tested
            ElmStructure.case' "case" "of" multiline
                (formatCommentedExpression elmVersion importInfo subject)
                (formatCaseClause elmVersion importInfo . extract . I.unFix2 <$> clauses)

        Tuple exprs multiline ->
            FormattedExpression SyntaxSeparated $
            ElmStructure.group True "(" "," ")" multiline $ formatCommentedExpression elmVersion importInfo <$> exprs

        TupleFunction n ->
            FormattedExpression SyntaxSeparated $
            keyword $ "(" <> Text.replicate (n-1) "," <> ")"

        Access expr field ->
            FormattedExpression SyntaxSeparated $
            formatExpression elmVersion importInfo expr
                |> syntaxParens SpaceSeparated -- TODO: does this need a different context than SpaceSeparated?
                |> ElmStructure.suffix (punc "." <> Box.identifier (Text.pack $ (\(LowercaseIdentifier l) -> l) $ fixLowercaseIdentifier elmVersion field))

        AccessFunction (LowercaseIdentifier field) ->
            FormattedExpression SyntaxSeparated $
            ElmStructure.identifier $ "." <> Text.pack (fixVarName elmVersion field)

        Record base fields trailing multiline ->
            FormattedExpression SyntaxSeparated $
            formatRecordLike
                (fmap (formatLowercaseIdentifier [] . fixLowercaseIdentifier elmVersion) <$> base)
                (formatPair "=" . mapPair (formatLowercaseIdentifier [] . fixLowercaseIdentifier elmVersion) (syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo) <$> fields)
                trailing multiline

        Parens expr ->
            case expr of
                C ([], []) expr' ->
                    formatExpression elmVersion importInfo expr'

                _ ->
                    FormattedExpression SyntaxSeparated $
                    formatCommentedExpression elmVersion importInfo expr
                        |> parens

        Unit comments ->
            FormattedExpression SyntaxSeparated $
            formatUnit '(' ')' comments

        GLShader src ->
          FormattedExpression SyntaxSeparated $
          ElmStructure.literal $ "[glsl|" <> Text.pack src <> "|]"


formatCaseClause :: ElmVersion -> ImportInfo [UppercaseIdentifier] -> ASTNS1 Identity [UppercaseIdentifier] 'CaseBranchNK -> Elm
formatCaseClause elmVersion importInfo (CaseBranch prePat postPat preExpr pat expr) =
    let
        (pat', forceArrowNewline) =
            case (prePat, postPat) of
                ([], []) ->
                    ( negativeCasePatternWorkaround pat $
                    syntaxParens SyntaxSeparated $
                    formatAst elmVersion importInfo pat
                    , False
                    )

                (prePat', []) ->
                    ( ElmStructure.stack1
                        [ ElmStructure.stack1 $ formatComment <$> prePat'
                        , negativeCasePatternWorkaround pat $
                          syntaxParens SyntaxSeparated $
                          formatAst elmVersion importInfo pat
                        ]
                    , False
                    )

                (prePat', postPat') ->
                    ( negativeCasePatternWorkaround pat $
                      formatCommentedStack (syntaxParens SyntaxSeparated . formatAst elmVersion importInfo <$> C (prePat', postPat') pat)
                    , True
                    )
    in
    ElmStructure.caseBranch "->" forceArrowNewline
        pat'
        (formatPreCommentedStack $ syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo <$> C preExpr expr)


formatCommentedExpression ::
    ElmVersion -> ImportInfo [UppercaseIdentifier]
    -> C2 before after (I.Fix2 Identity (ASTNS [UppercaseIdentifier]) 'ExpressionNK)
    -> Elm
formatCommentedExpression elmVersion importInfo (C (pre, post) e) =
    let
        commented' =
            case extract $ I.unFix2 e of
                Parens (C (pre'', post'') e'') ->
                    C (pre ++ pre'', post'' ++ post) e''
                _ -> C (pre, post) e
    in
    formatCommented $ syntaxParens SyntaxSeparated . formatExpression elmVersion importInfo <$> commented'


formatPreCommentedExpression ::
    Coapplicative annf =>
    ElmVersion -> ImportInfo [UppercaseIdentifier] -> SyntaxContext
    -> C1 before (I.Fix2 annf (ASTNS [UppercaseIdentifier]) 'ExpressionNK)
    -> Elm
formatPreCommentedExpression elmVersion importInfo context (C pre e) =
    let
        (pre', e') =
            case extract $ I.unFix2 e of
                Parens (C (pre'', []) e'') ->
                    (pre ++ pre'', e'')
                _ -> (pre, e)
    in
    formatCommented' pre' (syntaxParens context $ formatExpression elmVersion importInfo $ I.convert (Identity . extract) e')


formatRecordLike ::
    Maybe (C2 before after Elm) -> Sequence Elm -> Comments -> ForceMultiline
    -> Elm
formatRecordLike base' fields trailing multiline =
    case base' of
        Just base ->
            case sequenceToSetionedGroups fields trailing of
                Nothing ->
                    ElmStructure.spaceSepOrStack
                        (ElmStructure.spaceSepOrPrefix
                            (keyword "{")
                            (ElmStructure.spaceSepOrIndented (formatCommented base) [keyword "|"])
                        )
                        [keyword "}"]

                Just (firstGroup, moreGroups, extraFooter) ->
                    ElmStructure.extensionGroup "{" "|" "," "}"
                        ((\(ForceMultiline b) -> b) multiline)
                        (formatCommented base)
                        firstGroup moreGroups extraFooter

        Nothing->
            formatSequenceAsGroup '{' ',' '}'
                multiline
                trailing
                fields


sequenceToSetionedGroups :: Sequence Elm -> Comments -> Maybe (NonEmpty Elm, [(Elm, NonEmpty Elm)], Maybe Elm)
sequenceToSetionedGroups items trailing =
    let
        formatFirst (C (pre, post, eol) item) =
            formatC2Eol $ C (pre ++ post, [], eol) item

        formatItem (C (pre, post, eol) item) =
            case
                ( formatComments pre
                , formatC2Eol $ C (post, [], eol) item
                )
            of
                (Nothing, item') -> Right item'
                (Just pre, item') -> Left (pre, item')
    in
    case toCommentedList items of
        [] ->
            Nothing

        first:rest ->
            let
                (section1,sections) =
                    Data.Either.Extra.delimit $ formatItem <$> rest
            in
            Just
            ( formatFirst first :| section1
            , (\((a,b), c) -> (a, b:|c)) <$> sections
            , formatComments trailing
            )


formatSequenceAsGroup :: Char -> Char -> Char -> ForceMultiline -> Comments -> Sequence Elm -> Elm
formatSequenceAsGroup left delim right (ForceMultiline multiline) trailing items =
    case sequenceToSetionedGroups items trailing of
        Nothing ->
            formatUnit left right trailing

        Just (firstGroup, moreGroups, extraFooter) ->
            ElmStructure.sectionedGroup True
                (Text.singleton left) (Text.singleton delim) (Text.singleton right)
                multiline
                firstGroup moreGroups extraFooter


mapIsLast :: (Bool -> a -> b) -> [a] -> [b]
mapIsLast _ [] = []
mapIsLast f [last_] = [f True last_]
mapIsLast f (next:rest) = f False next : mapIsLast f rest


formatBinops ::
    ElmVersion
    -> ImportInfo [UppercaseIdentifier]
    -> I.Fix2 Identity (ASTNS [UppercaseIdentifier]) 'ExpressionNK
    -> [BinopsClause (Ref [UppercaseIdentifier]) (I.Fix2 Identity (ASTNS [UppercaseIdentifier]) 'ExpressionNK)]
    -> Bool
    -> Elm
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
            , formatInfixVar elmVersion o
            , formatCommented' pe $ syntaxParens formatContext $ formatExpression elmVersion importInfo e
            )
    in
        formatBinary
            multiline
            (syntaxParens InfixSeparated $ formatExpression elmVersion importInfo left)
            (mapIsLast formatPair_ ops)


formatRange_0_17 ::
    ElmVersion -> ImportInfo [UppercaseIdentifier]
    -> C2 before after (I.Fix2 Identity (ASTNS [UppercaseIdentifier]) 'ExpressionNK)
    -> C2 before after (I.Fix2 Identity (ASTNS [UppercaseIdentifier]) 'ExpressionNK)
    -> Elm
formatRange_0_17 elmVersion importInfo left right =
    ElmStructure.range "[" ".." "]"
        (formatCommentedExpression elmVersion importInfo left)
        (formatCommentedExpression elmVersion importInfo right)


formatRange_0_18 ::
    Coapplicative annf =>
    ElmVersion -> ImportInfo [UppercaseIdentifier]
    -> C2 before after (I.Fix2 annf (ASTNS [UppercaseIdentifier]) 'ExpressionNK)
    -> C2 before after (I.Fix2 annf (ASTNS [UppercaseIdentifier]) 'ExpressionNK)
    -> FormatResult 'ExpressionNK
formatRange_0_18 elmVersion importInfo left right =
    case (left, right) of
        (C (preLeft, []) left', C (preRight, []) right') ->
            App
                (I.Fix2 $ Identity $ VarExpr $ VarRef [UppercaseIdentifier "List"] $ LowercaseIdentifier "range")
                [ C preLeft $ I.convert (pure . extract) left'
                , C preRight $ I.convert (pure . extract) right'
                ]
                (FAJoinFirst JoinAll)
                |> (I.Fix2 . pure)
                |> formatExpression elmVersion importInfo

        _ ->
            App
                (I.Fix2 $ Identity $ VarExpr $ VarRef [UppercaseIdentifier "List"] $ LowercaseIdentifier "range")
                [ C [] $ I.Fix2 $ pure $ Parens $ I.convert (pure . extract) <$> left
                , C [] $ I.Fix2 $ pure $ Parens $ I.convert (pure . extract) <$> right
                ]
                (FAJoinFirst JoinAll)
                |> I.Fix2 . pure
                |> formatExpression elmVersion importInfo


formatUnit :: Char -> Char -> Comments -> Elm
formatUnit left right comments =
  case (left, comments) of
    (_, []) ->
        keyword $ Text.pack [left, right]

    ('{', (LineComment _):_) ->
      ElmStructure.groupOfOne "{ " (Text.singleton right) $
      ElmStructure.stack1 $ formatComment <$> comments

    (_, first:rest) ->
        ElmStructure.groupOfOne (Text.singleton left) (Text.singleton right) $
            ElmStructure.spaceSepOrStack (formatComment first) (formatComment <$> rest)


formatComments :: Comments -> Maybe Elm
formatComments comments =
    case fmap formatComment comments of
        [] ->
            Nothing

        (first:rest) ->
            Just $ ElmStructure.spaceSepOrStack first rest


formatCommented_ :: Bool -> C2 before after Elm -> Elm
formatCommented_ forceMultiline (C (pre, post) inner) =
    ElmStructure.forceableSpaceSepOrStack1 forceMultiline $
        concat
            [ maybeToList $ formatComments pre
            , [inner]
            , maybeToList $ formatComments post
            ]


formatCommented :: C2 before after Elm -> Elm
formatCommented =
    formatCommented_ False


formatPreCommented :: C1 before Elm -> Elm
formatPreCommented (C pre inner) =
    formatCommented (C (pre, []) inner)


formatCommented' :: Comments -> Elm -> Elm
formatCommented' pre inner =
    formatCommented (C (pre, []) inner)


formatTailCommented :: C1 after Elm -> Elm
formatTailCommented (C post inner) =
    formatCommented (C ([], post) inner)


formatC2Eol :: C2Eol before after Elm -> Elm
formatC2Eol (C (pre, post, eol) a) =
    formatCommented $ C (pre, post) $ formatEolCommented $ C eol a


formatEolCommented :: C0Eol Elm -> Elm
formatEolCommented (C Nothing inner) = inner
formatEolCommented (C (Just eol) inner) =
    ElmStructure.spaceSepMustBreak inner (formatComment $ LineComment eol)


formatCommentedStack :: C2 before after Elm -> Elm
formatCommentedStack (C (pre, post) inner) =
    ElmStructure.stack1 $
        fmap formatComment pre
        ++ [ inner ]
        ++ fmap formatComment post


formatPreCommentedStack :: C1 before Elm -> Elm
formatPreCommentedStack (C pre inner) =
    formatCommentedStack (C (pre, []) inner)


formatKeywordCommented :: String -> C2 beforeKeyword afterKeyword Elm -> Elm
formatKeywordCommented word (C (pre, post) value) =
    ElmStructure.spaceSepOrIndented
        (formatCommented $ keyword . Text.pack <$> C (pre, post) word)
        [ value ]


formatOpenCommentedList :: OpenCommentedList Elm -> [Elm]
formatOpenCommentedList (OpenCommentedList rest (C (preLst, eol) lst)) =
    fmap formatC2Eol rest
        ++ [formatC2Eol $ C (preLst, [], eol) lst]


formatComment :: Comment -> Elm
formatComment comment =
    case comment of
        BlockComment c ->
            ElmStructure.commentBlock "{-" "-}" (Text.pack <$> c)

        LineComment c ->
            ElmStructure.mustBreakComment ("--" <> Text.pack c)

        CommentTrickOpener ->
            ElmStructure.mustBreakComment "{--}"

        CommentTrickCloser ->
            ElmStructure.mustBreakComment "--}"

        CommentTrickBlock c ->
            ElmStructure.mustBreakComment ("{--" <> Text.pack c <> "-}")


formatLiteral :: ElmVersion -> LiteralValue -> Elm
formatLiteral elmVersion lit =
    case lit of
        IntNum i DecimalInt ->
            ElmStructure.literal $ Text.pack $ show i
        IntNum i HexadecimalInt ->
            ElmStructure.literal $ Text.pack $
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
            ElmStructure.literal $ Text.pack $ printf "%f" f
        FloatNum f ExponentFloat ->
            ElmStructure.literal $ Text.pack $ printf "%e" f
        Chr c ->
            formatString elmVersion SChar [c]
        Str s multi ->
            formatString elmVersion (SString multi) s
        Boolean b ->
            ElmStructure.literal $ Text.pack $ show b


data StringStyle
    = SChar
    | SString StringRepresentation
    deriving (Eq)


formatString :: ElmVersion -> StringStyle -> String -> Elm
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
            ElmStructure.literal $ mconcat
                [ quotes
                , Text.pack $ escaper $ concatMap fix s
                , quotes
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
              "\\u{" ++ (printf "%04X" $ Char.ord char) ++ "}"
          False ->
              "\\x" ++ (printf fmt $ Char.ord char)
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
                            step (next : (concat $ replicate quotes "\"\\") ++ okay) 0 rest
                        else if quotes > 0 then
                            step (next : (replicate quotes '"') ++ okay) 0 rest
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


typeParens :: TypeParensRequired -> (TypeParensInner, Elm) -> Elm
typeParens outer (inner, box) =
    if typeParensNeeded outer inner then parens box else box


typeParensNeeded :: TypeParensRequired -> TypeParensInner -> Bool
typeParensNeeded outer = \case
    NotNeeded -> False
    ForTypeConstruction -> outer >= ForCtor
    ForFunctionType -> outer >= ForLambda


commaSpace :: Line
commaSpace =
    punc "," <> space


formatTypeConstructor :: ElmVersion -> TypeConstructor ([UppercaseIdentifier], UppercaseIdentifier) -> Elm
formatTypeConstructor elmVersion ctor =
    case ctor of
        NamedConstructor (namespace, name) ->
            formatUppercaseIdentifier namespace $ fixUppercaseIdentifier elmVersion name

        TupleConstructor n ->
            keyword $ "(" <> Text.replicate (n-1) "," <> ")"


formatType ::
    Coapplicative annf =>
    ElmVersion -> I.Fix2 annf (ASTNS [UppercaseIdentifier]) 'TypeNK -> (TypeParensInner, Elm)
formatType elmVersion atype =
    case extract $ I.unFix2 atype of
        UnitType comments ->
          (,) NotNeeded $
          formatUnit '(' ')' comments

        FunctionType first rest (ForceMultiline forceMultiline) ->
            let
                formatRight (C (preOp, postOp, eol) term) =
                    ElmStructure.forceableSpaceSepOrStack1
                        False
                        $ concat
                            [ maybeToList $ formatComments preOp
                            , [ ElmStructure.prefixOrIndented
                                  (keyword "->")
                                  (formatC2Eol $
                                      (fmap $ typeParens ForLambda . formatType elmVersion)
                                      (C (postOp, [], eol) term)
                                  )
                              ]
                            ]
            in
                (,) ForFunctionType $
                ElmStructure.forceableSpaceSepOrStack
                    forceMultiline
                    (formatEolCommented (typeParens ForLambda . formatType elmVersion <$> first))
                    (formatRight <$> toCommentedList rest)

        TypeVariable var ->
            (,) NotNeeded $
            formatLowercaseIdentifier [] $ fixLowercaseIdentifier elmVersion var

        TypeConstruction ctor [] _ ->
            (,) NotNeeded $
            formatTypeConstructor elmVersion ctor

        TypeConstruction ctor (arg0:args) forceMultiline ->
            let
                join =
                    case forceMultiline of
                        ForceMultiline True -> FASplitFirst
                        ForceMultiline False -> FAJoinFirst JoinAll
            in
            (,) ForTypeConstruction $
            ElmStructure.application
                join
                (formatTypeConstructor elmVersion ctor)
                (formatPreCommented . fmap (typeParens ForCtor . formatType elmVersion) <$> arg0:|args)

        TypeParens type' ->
          (,) NotNeeded $
          parens $ formatCommented $ typeParens NotRequired . formatType elmVersion <$> type'

        TupleType types (ForceMultiline forceMultiline) ->
            (,) NotNeeded $
            ElmStructure.group True "(" "," ")" forceMultiline (formatC2Eol . fmap (typeParens NotRequired . formatType elmVersion) <$> NonEmpty.toList types)

        RecordType base fields trailing multiline ->
            (,) NotNeeded $
            formatRecordLike
                (fmap (formatLowercaseIdentifier [] . fixLowercaseIdentifier elmVersion) <$> base)
                (formatPair ":" . mapPair (formatLowercaseIdentifier [] . fixLowercaseIdentifier elmVersion) (typeParens NotRequired . formatType elmVersion) <$> fields)
                trailing multiline


formatVar :: ElmVersion -> Ref [UppercaseIdentifier] -> Elm
formatVar elmVersion var =
    case var of
        VarRef namespace name ->
            formatLowercaseIdentifier namespace $ fixLowercaseIdentifier elmVersion name

        TagRef namespace name ->
            formatUppercaseIdentifier namespace $ fixUppercaseIdentifier elmVersion name

        OpRef name ->
            formatSymbolIdentifierInParens name


formatSymbolIdentifierAsInfix :: SymbolIdentifier -> Elm
formatSymbolIdentifierAsInfix (SymbolIdentifier name) =
    ElmStructure.identifier $ Text.pack name


formatSymbolIdentifierInParens :: SymbolIdentifier -> Elm
formatSymbolIdentifierInParens (SymbolIdentifier name) =
    ElmStructure.identifier $ "(" <> Text.pack name <> ")"


formatInfixVar :: ElmVersion -> Ref [UppercaseIdentifier] -> Elm
formatInfixVar elmVersion var =
    case var of
        VarRef namespace name ->
            ElmStructure.groupOfOne "`" "`" $
            formatLowercaseIdentifier namespace $ fixLowercaseIdentifier elmVersion name

        TagRef namespace name ->
            ElmStructure.groupOfOne "`" "`" $
            formatUppercaseIdentifier namespace $ fixUppercaseIdentifier elmVersion name

        OpRef op ->
            formatSymbolIdentifierAsInfix op


formatQualifiedIdentifier :: [UppercaseIdentifier] -> Text -> Elm
formatQualifiedIdentifier namespace name =
    let
        namespace' = Text.pack . (\(UppercaseIdentifier n) -> n) <$> namespace
    in
    ElmStructure.identifier
        (Text.intercalate "." (namespace' ++ [name]))


formatLowercaseIdentifier :: [UppercaseIdentifier] -> LowercaseIdentifier -> Elm
formatLowercaseIdentifier namespace (LowercaseIdentifier name) =
    formatQualifiedIdentifier namespace (Text.pack name)


formatUppercaseIdentifier :: [UppercaseIdentifier] -> UppercaseIdentifier -> Elm
formatUppercaseIdentifier namespace (UppercaseIdentifier name) =
    formatQualifiedIdentifier namespace (Text.pack name)


formatUppercaseIdentifier' :: [UppercaseIdentifier] -> Elm
formatUppercaseIdentifier' [] =
    pleaseReport "UPEXPECTED UPPERCASE IDENTIFIER" "no name"
formatUppercaseIdentifier' some =
    formatUppercaseIdentifier (List.init some) (List.last some)


fixLowercaseIdentifier :: ElmVersion -> LowercaseIdentifier -> LowercaseIdentifier
fixLowercaseIdentifier elmVersion (LowercaseIdentifier name) =
    LowercaseIdentifier $ fixVarName elmVersion name


fixVarName :: ElmVersion -> String -> String
fixVarName elmVersion name =
    -- TODO: Move this to Normalize
    case elmVersion of
        Elm_0_16 -> name
        Elm_0_17 -> name
        _ -> map (\x -> if x == '\'' then '_' else x) name


fixUppercaseIdentifier :: ElmVersion -> UppercaseIdentifier -> UppercaseIdentifier
fixUppercaseIdentifier elmVersion (UppercaseIdentifier name) =
    UppercaseIdentifier $ fixVarName elmVersion name

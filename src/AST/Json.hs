{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module AST.Json where

import Elm.Utils ((|>))

import AST.Listing
import AST.MatchReferences (fromMatched, matchReferences)
import AST.Module
import AST.Structure
import AST.V0_16
import Data.Coapplicative
import Data.Foldable
import Data.Maybe (mapMaybe)
import Reporting.Annotation
import Text.JSON hiding (showJSON)

import qualified Data.Indexed as I
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified ElmFormat.Version
import qualified Reporting.Region as Region
import Data.ReversedList (Reversed)
import qualified Data.ReversedList as ReversedList
import qualified ElmFormat.AST.PublicAST as PublicAST
import ElmFormat.AST.PublicAST (ToJSON(..), type_, ModuleName(..))


pleaseReport :: String -> String -> a
pleaseReport what details =
    error ("<elm-format-" ++ ElmFormat.Version.asString ++ ": "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >")


showModule :: Module [UppercaseIdentifier] (ASTNS Located [UppercaseIdentifier] 'TopLevelNK) -> JSValue
showModule modu@(Module _ maybeHeader _ (C _ imports) body) =
    let
        header =
            maybeHeader
                |> Maybe.fromMaybe AST.Module.defaultHeader

        (Header _ (C _ name) _ _) = header

        importInfo =
            ImportInfo.fromModule mempty modu

        importJson (moduleName, C _ (ImportMethod alias (C _ exposing))) =
            let
                name = List.intercalate "." $ fmap (\(UppercaseIdentifier n) -> n) moduleName
            in
            ( name
            , makeObj
                [ ( "as", JSString $ toJSString $ maybe name (\(C _ (UppercaseIdentifier n)) -> n) alias)
                , ( "exposing", showImportListingJSON c exposing )
                ]
            )

        normalize =
            mapNs (fromMatched []) . matchReferences importInfo

        c =
            PublicAST.Config True
    in
    makeObj
        [ ( "moduleName", showJSON c $ ModuleName name )
        , ( "imports", makeObj $ fmap importJson $ Map.toList imports )
        , ( "body", JSArray $ fmap (showJSON c) $ mergeDeclarations $ normalize body )
        ]


data MergedTopLevelStructure ns
    = MergedDefinition
        { _definitionLocation :: Region.Region
        , _name_d :: LowercaseIdentifier
        , _args_d :: List (C1 Before (ASTNS Located ns 'PatternNK))
        , _preEquals :: Comments
        , _expression :: ASTNS Located ns 'ExpressionNK
        , _doc :: Maybe Comment
        , _annotation :: Maybe (Comments, Comments, ASTNS Located ns 'TypeNK)
        }
    | MergedTypeAlias
        { _location_ta :: Region.Region
        , _preAlias :: Comments
        , _name_ta :: UppercaseIdentifier
        , _args_ta :: [C1 BeforeTerm LowercaseIdentifier]
        , _type :: C1 AfterEquals (ASTNS Located ns 'TypeNK)
        }
    | TodoTopLevelStructure String


mergeDeclarations :: forall ns. Show ns => ASTNS Located ns 'TopLevelNK -> List (MergedTopLevelStructure ns)
mergeDeclarations (I.Fix (A _ (TopLevel decls))) =
    let
        collectAnnotation :: TopLevelStructure (ASTNS Located ns 'DeclarationNK) -> Maybe (LowercaseIdentifier, (Comments, Comments, ASTNS Located ns 'TypeNK))
        collectAnnotation decl =
            case fmap (extract . I.unFix) decl of
                Entry (TypeAnnotation (C preColon (VarRef () name)) (C postColon typ)) -> Just (name, (preColon, postColon, typ))
                _ -> Nothing

        annotations :: Map.Map LowercaseIdentifier (Comments, Comments, ASTNS Located ns 'TypeNK)
        annotations =
            Map.fromList $ mapMaybe collectAnnotation decls

        merge decl =
            case fmap I.unFix decl of
                Entry (A region (Definition (I.Fix (A _ (VarPattern name))) args preEquals expr)) ->
                    Just $ MergedDefinition
                        { _definitionLocation = region
                        , _name_d = name
                        , _args_d = args
                        , _preEquals = preEquals
                        , _expression = expr
                        , _doc = Nothing -- TODO: merge docs
                        , _annotation = Map.lookup name annotations
                        }

                Entry (A _ (TypeAnnotation _ _)) ->
                    -- TODO: retain annotations that don't have a matching definition
                    Nothing

                Entry (A region (TypeAlias c1 (C (c2, c3) (NameWithArgs name args)) type_)) ->
                    Just $ MergedTypeAlias
                        { _location_ta = region
                        , _preAlias = c1
                        , _name_ta = name
                        , _args_ta = args
                        , _type = type_
                        }

                _ ->
                    Just $ TodoTopLevelStructure (show decl)
    in
    mapMaybe merge decls


showImportListingJSON :: PublicAST.Config -> Listing DetailedListing -> JSValue
showImportListingJSON c (ExplicitListing a _) = showJSON c a
showImportListingJSON _ (OpenListing (C _ ())) = JSString $ toJSString "Everything"
showImportListingJSON _ ClosedListing = JSNull


showTagListingJSON :: Listing (CommentedMap UppercaseIdentifier ()) -> JSValue
showTagListingJSON (ExplicitListing tags _) =
    makeObj $ fmap (\(UppercaseIdentifier k, C _ ()) -> (k, JSBool True)) $ Map.toList tags
showTagListingJSON (OpenListing (C _ ())) = JSString $ toJSString "AllTags"
showTagListingJSON ClosedListing = JSString $ toJSString "NoTags"


instance ToJSON DetailedListing where
    showJSON _ (DetailedListing values _operators types) =
        makeObj
            [ ( "values", makeObj $ fmap (\(LowercaseIdentifier k) -> (k, JSBool True)) $ Map.keys values )
            , ( "types", makeObj $ fmap (\(UppercaseIdentifier k, (C _ (C _ listing))) -> (k, showTagListingJSON listing)) $ Map.toList types )
            ]


instance ToJSON (MergedTopLevelStructure [UppercaseIdentifier]) where
    showJSON _ (TodoTopLevelStructure what) =
        JSString $ toJSString ("TODO: " ++ what)
    showJSON c (MergedDefinition region name args _ expression doc annotation) =
        makeObj
            [ type_ "Definition"
            , ( "name" , showJSON c name )
            , ( "parameters", JSArray $ fmap (mergedParameter c) args )
            , ( "returnType", maybe JSNull (\(_, _, t) -> showJSON c $ PublicAST.fromRawAST t) annotation )
            , ( "expression" , showJSON c expression )
            , sourceLocation region
            ]
    showJSON c (MergedTypeAlias region preAlias name args (C comments t)) =
        makeObj
            [ type_ "TypeAlias"
            , ( "name", showJSON c name )
            , ( "type", showJSON c $ PublicAST.fromRawAST t)
            , sourceLocation region
            ]


mergedParameter :: PublicAST.Config -> C1 Before (ASTNS Located [UppercaseIdentifier] 'PatternNK) -> JSValue
mergedParameter c (C comments pattern) =
    makeObj
        [ ( "pattern", showJSON c $ PublicAST.fromRawAST pattern )
        , ( "type", JSNull ) -- TODO
        ]


-- instance ToJSON (TopLevelStructure Declaration) where
--   showJSON c (Entry (A region (Definition (A _ (VarPattern var)) _ _ expr))) =
--     makeObj
--       [ type_ "Definition"
--       , ( "name" , showJSON c var )
--       , ( "returnType", JSNull )
--       , ( "expression" , showJSON c expr )
--       , sourceLocation region
--       ]
--   showJSON _ _ = JSString $ toJSString "TODO: Decl"


instance ToJSON (ASTNS Located [UppercaseIdentifier] 'ExpressionNK) where
  showJSON c (I.Fix (A region expr)) =
      case expr of
          Unit comments ->
              makeObj [ type_ "UnitLiteral" ]

          Literal lit ->
              showJSON c (A region lit)

          VarExpr (VarRef [] (LowercaseIdentifier var)) ->
            variableReference region var

          VarExpr (VarRef namespace var) ->
              makeObj
                  [ type_ "ExternalReference"
                  , ("module", showJSON c $ ModuleName namespace)
                  , ("identifier", showJSON c var)
                  , sourceLocation region
                  ]

          VarExpr (TagRef [] (UppercaseIdentifier tag)) ->
            variableReference region tag

          VarExpr (TagRef namespace (UppercaseIdentifier var)) ->
              makeObj
                  [ type_ "ExternalReference"
                  , ("module", showJSON c $ ModuleName namespace)
                  , ("identifier", JSString $ toJSString var)
                  , sourceLocation region
                  ]

          VarExpr (OpRef (SymbolIdentifier sym)) ->
            variableReference region sym

          App expr args _ ->
              makeObj
                  [ type_ "FunctionApplication"
                  , ("function", showJSON c expr)
                  , ("arguments", JSArray $ showJSON c <$> map extract args)
                  ]

          Binops first rest _ ->
              makeObj
                  [ type_ "BinaryOperatorList"
                  , ("first", showJSON c first)
                  , ("operations"
                    , JSArray $ map
                        (\(BinopsClause _ op _ expr) ->
                           makeObj
                               [ ("operator", showJSON c $ (I.Fix $ noRegion $ VarExpr op :: ASTNS Located [UppercaseIdentifier] 'ExpressionNK))
                               , ("term", showJSON c expr)
                               ]
                        )
                        rest
                    )
                  ]

          Unary Negative expr ->
            makeObj
                [ type_ "UnaryOperator"
                , ("operator", JSString $ toJSString "-")
                , ("term", showJSON c expr)
                ]

          Parens (C comments expr) ->
              showJSON c expr

          ExplicitList terms _ _ ->
              makeObj
                  [ type_ "ListLiteral"
                  , ("terms", JSArray $ fmap (showJSON c) $ toList terms)
                  ]

          Tuple exprs _ ->
              makeObj
                  [ type_ "TupleLiteral"
                  , ("terms", JSArray $ fmap (showJSON c) (map extract exprs))
                  ]

          TupleFunction n | n <= 1 ->
            pleaseReport "INVALID TUPLE CONSTRUCTOR" ("n=" ++ show n)

          TupleFunction n ->
            variableReference region $ replicate (n-1) ','

          Record base fields _ _ ->
              recordJSON c "RecordLiteral" "RecordUpdate" base fields

          Access base field ->
            makeObj
                [ type_ "RecordAccess"
                , ( "record", showJSON c base )
                , ( "field", showJSON c field )
                ]

          Lambda parameters _ body _ ->
              makeObj
                  [ type_ "AnonymousFunction"
                  , ("parameters", JSArray $ fmap (showJSON c . PublicAST.fromRawAST . extract) parameters)
                  , ("body", showJSON c body)
                  ]

          If (IfClause (C _ cond') (C _ thenBody')) rest' (C _ elseBody) ->
              let
                  ifThenElse :: ToJSON e => e -> e -> [C1 Before (IfClause e)] -> JSValue
                  ifThenElse cond thenBody rest =
                      makeObj
                          [ type_ "IfExpression"
                          , ( "if", showJSON c cond )
                          , ( "then", showJSON c thenBody )
                          , ( "else"
                            , case rest of
                                [] ->
                                    showJSON c elseBody

                                C _ (IfClause (C _ nextCond) (C _ nextBody)) : nextRest ->
                                    ifThenElse nextCond nextBody nextRest
                            )
                          ]
              in
              ifThenElse cond' thenBody' rest'

          Let decls _ body ->
              makeObj
                  [ type_ "LetExpression"
                  , ( "declarations", JSArray $ map (showJSON c) decls)
                  , ("body", showJSON c body)
                  ]

          Case (C _ subject, _) branches ->
              makeObj
                  [ type_ "CaseExpression"
                  , ( "subject", showJSON c subject )
                  , ( "branches"
                    , JSArray $ map
                        (\(I.Fix (A _ (CaseBranch _ _ _ pat body))) ->
                           makeObj
                               [ ("pattern", showJSON c $ PublicAST.fromRawAST pat)
                               , ("body", showJSON c body)
                               ]
                        )
                        branches
                    )
                  ]

          Range _ _ _ ->
              JSString $ toJSString "TODO: Range"

          AccessFunction field ->
              makeObj
                  [ type_ "RecordAccess"
                  , ( "field", showJSON c field )
                  , sourceLocation region
                  ]

          GLShader _ ->
              JSString $ toJSString "TODO: GLShader"


recordJSON :: (ToJSON base, ToJSON value) => PublicAST.Config -> String -> String -> Maybe (C2 Before After base) -> Sequence (Pair LowercaseIdentifier value) -> JSValue
recordJSON c tag extensionTag base fields =
    let
        fieldsJSON =
            ( "fields"
            , makeObj $ fmap
                (\(Pair (C _ (LowercaseIdentifier key)) (C _ value) _) ->
                    (key, showJSON c value)
                )
                $ toList fields
            )

        fieldOrder =
            ( "fieldOrder"
            , JSArray $
                fmap (\(Pair (C _ key) _ _) -> showJSON c key) $
                toList fields
            )
    in
    case base of
        Just (C _ identifier) ->
            makeObj
                [ type_ extensionTag
                , ("base", showJSON c identifier)
                , fieldsJSON
                , ( "display"
                , makeObj
                    [ fieldOrder
                    ]
                )
                ]

        Nothing ->
            makeObj
                [ type_ tag
                , fieldsJSON
                , ( "display"
                , makeObj
                    [ fieldOrder
                    ]
                )
                ]


variableReference :: Region.Region -> String -> JSValue
variableReference region name =
    makeObj
        [ type_ "VariableReference"
        , ( "name" , JSString $ toJSString name )
        , sourceLocation region
        ]


sourceLocation :: Region.Region -> (String, JSValue)
sourceLocation region =
    ( "sourceLocation", showJSON undefined region )


instance ToJSON (ASTNS Located [UppercaseIdentifier] 'LetDeclarationNK) where
  showJSON c letDeclaration =
      case extract $ I.unFix letDeclaration of
          LetDefinition (I.Fix (A _ (VarPattern var))) [] _ expr ->
              makeObj
                  [ type_ "Definition"
                  , ("name" , showJSON c var)
                  , ("expression" , showJSON c expr)
                  ]

          _ ->
              JSString $ toJSString $ "TODO: LetDeclaration (" ++ show letDeclaration ++ ")"


nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> Located a
noRegion =
    at nowhere nowhere

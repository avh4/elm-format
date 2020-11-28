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
                , ( "exposing", showImportListingJSON exposing )
                ]
            )

        normalize =
            mapNs (fromMatched []) . matchReferences importInfo
    in
    makeObj
        [ ( "moduleName", showJSON name )
        , ( "imports", makeObj $ fmap importJson $ Map.toList imports )
        , ( "body", JSArray $ fmap showJSON $ mergeDeclarations $ normalize body )
        ]


data MergedTopLevelStructure ns
    = MergedDefinition
        { _definitionLocation :: Region.Region
        , _name :: LowercaseIdentifier
        , _args :: List (C1 Before (ASTNS Located ns 'PatternNK))
        , _preEquals :: Comments
        , _expression :: ASTNS Located ns 'ExpressionNK
        , _doc :: Maybe Comment
        , _annotation :: Maybe (Comments, Comments, ASTNS Located ns 'TypeNK)
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
                        , _name = name
                        , _args = args
                        , _preEquals = preEquals
                        , _expression = expr
                        , _doc = Nothing -- TODO: merge docs
                        , _annotation = Map.lookup name annotations
                        }

                Entry (A _ (TypeAnnotation _ _)) ->
                    -- TODO: retain annotations that don't have a matching definition
                    Nothing

                _ ->
                    Just $ TodoTopLevelStructure (show decl)
    in
    mapMaybe merge decls


class ToJSON a where
  showJSON :: a -> JSValue


instance ToJSON Region.Region where
    showJSON region =
        makeObj
            [ ( "start", showJSON $ Region.start region )
            , ( "end", showJSON $ Region.end region )
            ]


instance ToJSON Region.Position where
    showJSON pos =
        makeObj
            [ ( "line", JSRational False $ toRational $ Region.line pos )
            , ( "col", JSRational False $ toRational $ Region.column pos )
            ]


instance ToJSON (List UppercaseIdentifier) where
    showJSON [] = JSNull
    showJSON namespace = (JSString . toJSString . List.intercalate "." . fmap (\(UppercaseIdentifier v) -> v)) namespace


instance ToJSON UppercaseIdentifier where
    showJSON (UppercaseIdentifier name) = JSString $ toJSString name


showImportListingJSON :: Listing DetailedListing -> JSValue
showImportListingJSON (ExplicitListing a _) = showJSON a
showImportListingJSON (OpenListing (C _ ())) = JSString $ toJSString "Everything"
showImportListingJSON ClosedListing = JSNull


showTagListingJSON :: Listing (CommentedMap UppercaseIdentifier ()) -> JSValue
showTagListingJSON (ExplicitListing tags _) =
    makeObj $ fmap (\(UppercaseIdentifier k, C _ ()) -> (k, JSBool True)) $ Map.toList tags
showTagListingJSON (OpenListing (C _ ())) = JSString $ toJSString "AllTags"
showTagListingJSON ClosedListing = JSString $ toJSString "NoTags"


instance ToJSON DetailedListing where
    showJSON (DetailedListing values _operators types) =
        makeObj
            [ ( "values", makeObj $ fmap (\(LowercaseIdentifier k) -> (k, JSBool True)) $ Map.keys values )
            , ( "types", makeObj $ fmap (\(UppercaseIdentifier k, (C _ (C _ listing))) -> (k, showTagListingJSON listing)) $ Map.toList types )
            ]


instance ToJSON (MergedTopLevelStructure [UppercaseIdentifier]) where
    showJSON (TodoTopLevelStructure what) =
        JSString $ toJSString ("TODO: " ++ what)
    showJSON (MergedDefinition region (LowercaseIdentifier name) args _ expression doc annotation) =
        makeObj
            [ type_ "Definition"
            , ( "name" , JSString $ toJSString name )
            , ( "returnType", maybe JSNull (\(_, _, t) -> showJSON t) annotation )
            , ( "expression" , showJSON expression )
            , sourceLocation region
            ]

-- instance ToJSON (TopLevelStructure Declaration) where
--   showJSON (Entry (A region (Definition (A _ (VarPattern (LowercaseIdentifier var))) _ _ expr))) =
--     makeObj
--       [ type_ "Definition"
--       , ( "name" , JSString $ toJSString var )
--       , ( "returnType", JSNull )
--       , ( "expression" , showJSON expr )
--       , sourceLocation region
--       ]
--   showJSON _ = JSString $ toJSString "TODO: Decl"


instance ToJSON (ASTNS Located [UppercaseIdentifier] 'ExpressionNK) where
  showJSON (I.Fix (A region expr)) =
      case expr of
          Unit _ ->
              makeObj [ type_ "UnitLiteral" ]

          Literal (IntNum value repr) ->
              makeObj
                  [ type_ "IntLiteral"
                  , ("value", JSRational False $ toRational value)
                  , ("display"
                    , makeObj
                        [ ("representation", showJSON repr)
                        ]
                    )
                  ]

          Literal (FloatNum value repr) ->
              makeObj
                  [ type_ "FloatLiteral"
                  , ("value", JSRational False $ toRational value)
                  , ("display"
                    , makeObj
                        [ ("representation", showJSON repr)
                        ]
                    )
                  ]

          Literal (Boolean value) ->
            makeObj
                [ type_ "ExternalReference"
                , ("module", JSString $ toJSString "Basics")
                , ("identifier", JSString $ toJSString $ show value)
                , sourceLocation region
                ]

          Literal (Chr chr) ->
              makeObj
                  [ type_ "CharLiteral"
                  , ("module", JSString $ toJSString "Char")
                  , ("value", JSString $ toJSString [chr])
                  , sourceLocation region
                  ]

          Literal (Str str _) ->
              makeObj
                  [ type_ "StringLiteral"
                  , ("module", JSString $ toJSString "String")
                  , ("value", JSString $ toJSString str)
                  , sourceLocation region
                  ]

          VarExpr (VarRef [] (LowercaseIdentifier var)) ->
            variableReference region var

          VarExpr (VarRef namespace (LowercaseIdentifier var)) ->
              makeObj
                  [ type_ "ExternalReference"
                  , ("module"
                    , showJSON namespace)
                  , ("identifier", JSString $ toJSString var)
                  , sourceLocation region
                  ]

          VarExpr (TagRef [] (UppercaseIdentifier tag)) ->
            variableReference region tag

          VarExpr (TagRef namespace (UppercaseIdentifier var)) ->
              makeObj
                  [ type_ "ExternalReference"
                  , ("module"
                    , showJSON namespace)
                  , ("identifier", JSString $ toJSString var)
                  , sourceLocation region
                  ]

          VarExpr (OpRef (SymbolIdentifier sym)) ->
            variableReference region sym

          VarExpr _ ->
              JSString $ toJSString "TODO: VarExpr"

          App expr args _ ->
              makeObj
                  [ type_ "FunctionApplication"
                  , ("function", showJSON expr)
                  , ("arguments", JSArray $ showJSON <$> map extract args)
                  ]

          Binops first rest _ ->
              makeObj
                  [ type_ "BinaryOperatorList"
                  , ("first", showJSON first)
                  , ("operations"
                    , JSArray $ map
                        (\(BinopsClause _ op _ expr) ->
                           makeObj
                               [ ("operator", showJSON $ (I.Fix $ noRegion $ VarExpr op :: ASTNS Located [UppercaseIdentifier] 'ExpressionNK))
                               , ("term", showJSON expr)
                               ]
                        )
                        rest
                    )
                  ]

          Unary Negative expr ->
            makeObj
                [ type_ "UnaryOperator"
                , ("operator", JSString $ toJSString "-")
                , ("term", showJSON expr)
                ]

          Parens (C _ expr) ->
              showJSON expr

          ExplicitList terms _ _ ->
              makeObj
                  [ type_ "ListLiteral"
                  , ("terms", JSArray $ fmap showJSON $ toList terms)
                  ]

          Tuple exprs _ ->
              makeObj
                  [ type_ "TupleLiteral"
                  , ("terms", JSArray $ fmap showJSON (map extract exprs))
                  ]

          TupleFunction n | n <= 1 ->
            pleaseReport "INVALID TUPLE CONSTRUCTOR" ("n=" ++ show n)

          TupleFunction n ->
            variableReference region $ replicate (n-1) ','

          Record base fields _ _ ->
              let
                  fieldsJSON =
                      ( "fields"
                      , makeObj $ fmap
                          (\(Pair (C _ (LowercaseIdentifier key)) (C _ value) _) ->
                             (key, showJSON value)
                          )
                          $ toList fields
                      )

                  fieldOrder =
                      ( "fieldOrder"
                      , JSArray $
                            fmap (JSString . toJSString) $
                            fmap (\(Pair (C _ (LowercaseIdentifier key)) _ _) -> key) $
                            toList fields
                      )
              in
              case base of
                  Just (C _ (LowercaseIdentifier identifier)) ->
                      makeObj
                          [ type_ "RecordUpdate"
                          , ("base", JSString $ toJSString identifier)
                          , fieldsJSON
                          , ( "display"
                            , makeObj
                                [ fieldOrder
                                ]
                            )
                          ]

                  Nothing ->
                      makeObj
                          [ type_ "RecordLiteral"
                          , fieldsJSON
                          , ( "display"
                            , makeObj
                                [ fieldOrder
                                ]
                            )
                          ]

          Access base (LowercaseIdentifier field) ->
            makeObj
                [ type_ "RecordAccess"
                , ( "record", showJSON base )
                , ( "field", JSString $ toJSString field )
                ]

          Lambda parameters _ body _ ->
              makeObj
                  [ type_ "AnonymousFunction"
                  , ("parameters", JSArray $ fmap (showJSON . extract) parameters)
                  , ("body", showJSON body)
                  ]

          If (IfClause (C _ cond') (C _ thenBody')) rest' (C _ elseBody) ->
              let
                  ifThenElse :: ToJSON e => e -> e -> [C1 Before (IfClause e)] -> JSValue
                  ifThenElse cond thenBody rest =
                      makeObj
                          [ type_ "IfExpression"
                          , ( "if", showJSON cond )
                          , ( "then", showJSON thenBody )
                          , ( "else"
                            , case rest of
                                [] ->
                                    showJSON elseBody

                                C _ (IfClause (C _ nextCond) (C _ nextBody)) : nextRest ->
                                    ifThenElse nextCond nextBody nextRest
                            )
                          ]
              in
              ifThenElse cond' thenBody' rest'

          Let decls _ body ->
              makeObj
                  [ type_ "LetExpression"
                  , ( "declarations", JSArray $ map showJSON decls)
                  , ("body", showJSON body)
                  ]

          Case (C _ subject, _) branches ->
              makeObj
                  [ type_ "CaseExpression"
                  , ( "subject", showJSON subject )
                  , ( "branches"
                    , JSArray $ map
                        (\(I.Fix (A _ (CaseBranch _ _ _ pat body))) ->
                           makeObj
                               [ ("pattern", showJSON pat)
                               , ("body", showJSON body)
                               ]
                        )
                        branches
                    )
                  ]

          Range _ _ _ ->
              JSString $ toJSString "TODO: Range"

          AccessFunction _ ->
              JSString $ toJSString "TODO: AccessFunction"

          GLShader _ ->
              JSString $ toJSString "TODO: GLShader"


variableReference :: Region.Region -> String -> JSValue
variableReference region name =
    makeObj
        [ type_ "VariableReference"
        , ( "name" , JSString $ toJSString name )
        , sourceLocation region
        ]


sourceLocation :: Region.Region -> (String, JSValue)
sourceLocation region =
    ( "sourceLocation", showJSON region )


instance ToJSON (ASTNS Located [UppercaseIdentifier] 'LetDeclarationNK) where
  showJSON letDeclaration =
      case extract $ I.unFix letDeclaration of
          LetDefinition (I.Fix (A _ (VarPattern (LowercaseIdentifier var)))) [] _ expr ->
              makeObj
                  [ type_ "Definition"
                  , ("name" , JSString $ toJSString var)
                  , ("expression" , showJSON expr)
                  ]

          _ ->
              JSString $ toJSString $ "TODO: LetDeclaration (" ++ show letDeclaration ++ ")"


instance ToJSON IntRepresentation where
    showJSON DecimalInt = JSString $ toJSString $ "DecimalInt"
    showJSON HexadecimalInt = JSString $ toJSString $ "HexadecimalInt"


instance ToJSON FloatRepresentation where
    showJSON DecimalFloat = JSString $ toJSString "DecimalFloat"
    showJSON ExponentFloat = JSString $ toJSString "ExponentFloat"


instance ToJSON (ASTNS Located [UppercaseIdentifier] 'PatternNK) where
  showJSON pattern' =
      JSString $ toJSString $ "TODO: Pattern (" ++ show pattern' ++ ")"


instance ToJSON (ASTNS Located [UppercaseIdentifier] 'TypeNK) where
    showJSON type' =
        case extract $ I.unFix type' of
            TypeConstruction (NamedConstructor (namespace, name)) args forceMultine ->
                makeObj
                    [ type_ "TypeReference"
                    , ( "name", showJSON name )
                    , ( "module", showJSON namespace )
                    , ( "arguments", JSArray $ fmap (showJSON . extract) args )
                    ]

            TypeVariable (LowercaseIdentifier name) ->
                makeObj
                    [ type_ "TypeVariable"
                    , ( "name", JSString $ toJSString name )
                    ]

            FunctionType first rest _ ->
                case firstRestToRestLast first (sequenceToList rest) of
                    (args, C _ last) ->
                        makeObj
                            [ type_ "FunctionType"
                            , ( "returnType", showJSON last)
                            , ( "argumentTypes", JSArray $ fmap (\(C _ t) -> showJSON t) $ args )
                            ]

            _ ->
                JSString $ toJSString $ "TODO: Type (" ++ show type' ++ ")"
        where
            firstRestToRestLast :: C0Eol x -> List (C2Eol a b x) -> (List (C2Eol a b x), C0Eol x)
            firstRestToRestLast first rest =
                done $ foldl (flip step) (ReversedList.empty, first) rest
                where
                    step :: C2Eol a b x -> (Reversed (C2Eol a b x), C0Eol x) -> (Reversed (C2Eol a b x), C0Eol x)
                    step (C (a, b, dn) next) (acc, C dn' last) =
                        (ReversedList.push (C (a, b, dn') last) acc, C dn next)

                    done :: (Reversed (C2Eol a b x), C0Eol x) -> (List (C2Eol a b x), C0Eol x)
                    done (acc, last) =
                        (ReversedList.toList acc, last)


type_ :: String -> (String, JSValue)
type_ t =
    ("type", JSString $ toJSString t)


nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> Located a
noRegion =
    at nowhere nowhere

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module AST.Json where

import Elm.Utils ((|>))

import AST.Declaration
import AST.Expression
import AST.MapNamespace
import AST.Module
import AST.Pattern
import AST.Variable
import AST.V0_16
import Reporting.Annotation hiding (map)
import Text.JSON hiding (showJSON)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified ElmFormat.Version
import qualified Reporting.Region as Region


pleaseReport :: String -> String -> a
pleaseReport what details =
    error ("<elm-format-" ++ ElmFormat.Version.asString ++ ": "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >")


showModule :: Module -> JSValue
showModule (Module _ maybeHeader _ (_, imports) body) =
    let
        header =
            maybeHeader
                |> Maybe.fromMaybe AST.Module.defaultHeader

        (Header _ (Commented _ name _) _ _) = header

        getAlias :: ImportMethod -> Maybe UppercaseIdentifier
        getAlias importMethod =
            fmap (snd . snd) (alias importMethod)

        importAliases =
            Map.fromList $ map (\(k, v) -> (v, k)) $ Map.toList $ Map.mapMaybe (getAlias . snd) imports

        normalizeNamespace namespace =
            case namespace of
                [alias] ->
                    Map.findWithDefault namespace alias importAliases

                _ ->
                    namespace

        importJson (moduleName, (_, ImportMethod alias (_, (_, exposing)))) =
            let
                name = List.intercalate "." $ fmap (\(UppercaseIdentifier n) -> n) moduleName
            in
            ( name
            , makeObj
                [ ( "as", JSString $ toJSString $ maybe name (\(_, (_, UppercaseIdentifier n)) -> n) alias)
                , ( "exposing", showImportListingJSON exposing )
                ]
            )
    in
    makeObj
        [ ( "moduleName", showJSON name )
        , ( "imports", makeObj $ fmap importJson $ Map.toList imports )
        , ( "body" , JSArray $ fmap showJSON $ mapNamespace normalizeNamespace body )
        ]


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
    showJSON = JSString . toJSString . List.intercalate "." . fmap (\(UppercaseIdentifier v) -> v)


showImportListingJSON :: Listing DetailedListing -> JSValue
showImportListingJSON (ExplicitListing a _) = showJSON a
showImportListingJSON (OpenListing (Commented _ () _)) = JSString $ toJSString "Everything"
showImportListingJSON ClosedListing = JSNull


showTagListingJSON :: Listing (CommentedMap UppercaseIdentifier ()) -> JSValue
showTagListingJSON (ExplicitListing tags _) =
    makeObj $ fmap (\(UppercaseIdentifier k, Commented _ () _) -> (k, JSBool True)) $ Map.toList tags
showTagListingJSON (OpenListing (Commented _ () _)) = JSString $ toJSString "AllTags"
showTagListingJSON ClosedListing = JSString $ toJSString "NoTags"


instance ToJSON DetailedListing where
    showJSON (DetailedListing values _operators types) =
        makeObj
            [ ( "values", makeObj $ fmap (\(LowercaseIdentifier k) -> (k, JSBool True)) $ Map.keys values )
            , ( "types", makeObj $ fmap (\(UppercaseIdentifier k, (Commented _ (_, listing) _)) -> (k, showTagListingJSON listing)) $ Map.toList types )
            ]


instance ToJSON (TopLevelStructure Declaration) where
  showJSON (Entry (A region (Definition (A _ (VarPattern (LowercaseIdentifier var))) _ _ expr))) =
    makeObj
      [ type_ "Definition"
      , ("name" , JSString $ toJSString var)
      , ("expression" , showJSON expr)
      , sourceLocation region
      ]
  showJSON _ = JSString $ toJSString "TODO: Decl"


instance ToJSON Expr where
  showJSON (A region expr) =
      case expr of
          Unit _ ->
              makeObj [ type_ "UnitLiteral" ]

          AST.Expression.Literal (IntNum value repr) ->
              makeObj
                  [ type_ "IntLiteral"
                  , ("value", JSRational False $ toRational value)
                  , ("display"
                    , makeObj
                        [ ("representation", showJSON repr)
                        ]
                    )
                  ]

          AST.Expression.Literal (FloatNum value repr) ->
              makeObj
                  [ type_ "FloatLiteral"
                  , ("value", JSRational False $ toRational value)
                  , ("display"
                    , makeObj
                        [ ("representation", showJSON repr)
                        ]
                    )
                  ]

          AST.Expression.Literal (Boolean value) ->
            makeObj
                [ type_ "ExternalReference"
                , ("module", JSString $ toJSString "Basics")
                , ("identifier", JSString $ toJSString $ show value)
                , sourceLocation region
                ]

          AST.Expression.Literal (Chr chr) ->
              makeObj
                  [ type_ "CharLiteral"
                  , ("module", JSString $ toJSString "Char")
                  , ("value", JSString $ toJSString [chr])
                  , sourceLocation region
                  ]

          AST.Expression.Literal (Str str _) ->
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

          VarExpr (OpRef (SymbolIdentifier sym)) ->
            variableReference region sym

          VarExpr _ ->
              JSString $ toJSString "TODO: VarExpr"

          App expr args _ ->
              makeObj
                  [ type_ "FunctionApplication"
                  , ("function", showJSON expr)
                  , ("arguments", JSArray $ showJSON <$> map (\(_ , arg) -> arg) args)
                  ]

          Binops first rest _ ->
              makeObj
                  [ type_ "BinaryOperatorList"
                  , ("first", showJSON first)
                  , ("operations"
                    , JSArray $ map
                        (\(_, op, _, expr) ->
                           makeObj
                               [ ("operator", showJSON $ noRegion $ VarExpr op)
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

          Parens (Commented _ expr _) ->
              showJSON expr

          ExplicitList terms _ _ ->
              makeObj
                  [ type_ "ListLiteral"
                  , ("terms", JSArray $ fmap showJSON (map (\(_, (_, (term, _))) -> term) terms))
                  ]

          AST.Expression.Tuple exprs _ ->
              makeObj
                  [ type_ "TupleLiteral"
                  , ("terms", JSArray $ fmap showJSON (map (\(Commented _ expr _) -> expr) exprs))
                  ]

          TupleFunction n | n <= 1 ->
            pleaseReport "INVALID TUPLE CONSTRUCTOR" ("n=" ++ show n)

          TupleFunction n ->
            variableReference region $ replicate (n-1) ','

          AST.Expression.Record base fields _ _ ->
              let
                  fieldsJSON =
                      ( "fields"
                      , makeObj $ fmap
                          (\(_, (_, (Pair (LowercaseIdentifier key, _) (_, value) _, _))) ->
                             (key, showJSON value)
                          )
                          fields
                      )

                  fieldOrder =
                      ( "fieldOrder"
                      , JSArray $
                            fmap (JSString . toJSString) $
                            fmap (\(_, (_, (Pair (LowercaseIdentifier key, _) _ _, _))) -> key) fields
                      )
              in
              case base of
                  Just (Commented _ (LowercaseIdentifier identifier) _) ->
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
                  , ("parameters", JSArray $ map (\(_, A _ pat) -> showJSON pat) parameters)
                  , ("body", showJSON body)
                  ]

          If (Commented _ cond' _, Commented _ thenBody' _) rest' (_, elseBody) ->
              let
                  ifThenElse :: Expr -> Expr -> [(Comments, IfClause)] -> JSValue
                  ifThenElse cond thenBody rest =
                      makeObj
                          [ type_ "IfExpression"
                          , ( "if", showJSON cond )
                          , ( "then", showJSON thenBody )
                          , ( "else"
                            , case rest of
                                [] ->
                                    showJSON elseBody

                                (_, (Commented _ nextCond _, Commented _ nextBody _)) : nextRest ->
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

          Case (Commented _ subject _, _) branches ->
              makeObj
                  [ type_ "CaseExpression"
                  , ( "subject", showJSON subject )
                  , ( "branches"
                    , JSArray $ map
                        (\(Commented _ (A _ pat) _, (_, body)) ->
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


instance ToJSON LetDeclaration where
  showJSON letDeclaration =
      case letDeclaration of
          LetDefinition (A _ (VarPattern (LowercaseIdentifier var))) [] _ expr ->
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


instance ToJSON Pattern' where
  showJSON pattern' =
      JSString $ toJSString $ "TODO: Pattern (" ++ show pattern' ++ ")"


type_ :: String -> (String, JSValue)
type_ t =
    ("type", JSString $ toJSString t)


nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> Located a
noRegion =
    at nowhere nowhere

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module AST.Json where

import AST.Declaration
import AST.Expression
import AST.Module
import AST.Pattern
import AST.Variable
import AST.V0_16
import Reporting.Annotation hiding (map)
import Text.JSON hiding (showJSON)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified ElmFormat.Version
import qualified Reporting.Region as Region


pleaseReport :: String -> String -> a
pleaseReport what details =
    error ("<elm-format-" ++ ElmFormat.Version.asString ++ ": "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >")


showModule :: Module -> JSValue
showModule (Module _ _ _ (_, imports) body) =
    let
        getAlias :: ImportMethod -> Maybe UppercaseIdentifier
        getAlias importMethod =
            fmap (snd . snd) (alias importMethod)

        importAliases =
            Map.fromList $ map (\(k, v) -> (v, k)) $ Map.toList $ Map.mapMaybe (getAlias . snd) imports
    in
    makeObj [ ("body" , JSArray $ fmap (showJSON importAliases) body) ]


class ToJSON a where
  showJSON :: Map.Map UppercaseIdentifier [UppercaseIdentifier] -> a -> JSValue


instance ToJSON Region.Region where
    showJSON x region =
        makeObj
            [ ( "start", showJSON x $ Region.start region )
            , ( "end", showJSON x $ Region.end region )
            ]


instance ToJSON Region.Position where
    showJSON _ pos =
        makeObj
            [ ( "line", JSRational False $ toRational $ Region.line pos )
            , ( "col", JSRational False $ toRational $ Region.column pos )
            ]


instance ToJSON (TopLevelStructure Declaration) where
  showJSON importAliases (Entry (A _ (Definition (A _ (VarPattern (LowercaseIdentifier var))) _ _ expr))) =
    makeObj
      [ type_ "Definition"
      , ("name" , JSString $ toJSString var)
      , ("expression" , showJSON importAliases expr)
      ]
  showJSON _ _ = JSString $ toJSString "TODO: Decl"


instance ToJSON Expr where
  showJSON importAliases (A region expr) =
      case expr of
          Unit _ ->
              makeObj [ type_ "UnitLiteral" ]

          AST.Expression.Literal (IntNum value repr) ->
              makeObj
                  [ type_ "IntLiteral"
                  , ("value", JSRational False $ toRational value)
                  , ("display"
                    , makeObj
                        [ ("representation", showJSON importAliases repr)
                        ]
                    )
                  ]

          AST.Expression.Literal (Boolean value) ->
            makeObj
                [ type_ "ExternalReference"
                , ("module", JSString $ toJSString "Basics")
                , ("identifier", JSString $ toJSString $ show value)
                , sourceLocation importAliases region
                ]

          VarExpr (VarRef [] (LowercaseIdentifier var)) ->
            variableReference importAliases region var

          VarExpr (VarRef namespace (LowercaseIdentifier var)) ->
              let
                  normalizedNamespace =
                      case namespace of
                          [alias] ->
                              Map.findWithDefault namespace alias importAliases

                          _ ->
                              namespace
              in
              makeObj
                  [ type_ "ExternalReference"
                  , ("module"
                    , JSString $ toJSString $ List.intercalate "." $ map (\(UppercaseIdentifier v) -> v) normalizedNamespace)
                  , ("identifier", JSString $ toJSString var)
                  , sourceLocation importAliases region
                  ]

          VarExpr (TagRef [] (UppercaseIdentifier tag)) ->
            variableReference importAliases region tag

          VarExpr (OpRef (SymbolIdentifier sym)) ->
            variableReference importAliases region sym

          App expr args _ ->
              makeObj
                  [ type_ "FunctionApplication"
                  , ("function", showJSON importAliases expr)
                  , ("arguments", JSArray $ showJSON importAliases <$> map (\(_ , arg) -> arg) args)
                  ]

          Binops first rest _ ->
              makeObj
                  [ type_ "BinaryOperatorList"
                  , ("first", showJSON importAliases first)
                  , ("operations"
                    , JSArray $ map
                        (\(_, op, _, expr) ->
                           makeObj
                               [ ("operator", showJSON importAliases $ noRegion $ VarExpr op)
                               , ("term", showJSON importAliases expr)
                               ]
                        )
                        rest
                    )
                  ]

          Unary Negative expr ->
            makeObj
                [ type_ "UnaryOperator"
                , ("operator", JSString $ toJSString "-")
                , ("term", showJSON importAliases expr)
                ]

          Parens (Commented _ expr _) ->
              showJSON importAliases expr

          ExplicitList terms _ _ ->
              makeObj
                  [ type_ "ListLiteral"
                  , ("terms", JSArray $ fmap (showJSON importAliases) (map (\(_, (_, (term, _))) -> term) terms))
                  ]

          AST.Expression.Tuple exprs _ ->
              makeObj
                  [ type_ "TupleLiteral"
                  , ("terms", JSArray $ fmap (showJSON importAliases) (map (\(Commented _ expr _) -> expr) exprs))
                  ]

          TupleFunction n | n <= 1 ->
            pleaseReport "INVALID TUPLE CONSTRUCTOR" ("n=" ++ show n)

          TupleFunction n ->
            variableReference importAliases region $ replicate (n-1) ','

          AST.Expression.Record base fields _ _ ->
              let
                  fieldsJSON =
                      ( "fields"
                      , makeObj $ fmap
                          (\(_, (_, (Pair (LowercaseIdentifier key, _) (_, value) _, _))) ->
                             (key, showJSON importAliases value)
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
                , ( "record", showJSON importAliases base )
                , ( "field", JSString $ toJSString field )
                ]

          Lambda parameters _ body _ ->
              makeObj
                  [ type_ "AnonymousFunction"
                  , ("parameters", JSArray $ map (\(_, A _ pat) -> showJSON importAliases pat) parameters)
                  , ("body", showJSON importAliases body)
                  ]

          If (Commented _ cond' _, Commented _ thenBody' _) rest' (_, elseBody) ->
              let
                  ifThenElse :: Expr -> Expr -> [(Comments, IfClause)] -> JSValue
                  ifThenElse cond thenBody rest =
                      makeObj
                          [ type_ "IfExpression"
                          , ( "if", showJSON importAliases cond )
                          , ( "then", showJSON importAliases thenBody )
                          , ( "else"
                            , case rest of
                                [] ->
                                    showJSON importAliases elseBody

                                (_, (Commented _ nextCond _, Commented _ nextBody _)) : nextRest ->
                                    ifThenElse nextCond nextBody nextRest
                            )
                          ]
              in
              ifThenElse cond' thenBody' rest'

          Let decls _ body ->
              makeObj
                  [ type_ "LetExpression"
                  , ( "declarations", JSArray $ map (showJSON importAliases) decls)
                  , ("body", showJSON importAliases body)
                  ]

          Case (Commented _ subject _, _) branches ->
              makeObj
                  [ type_ "CaseExpression"
                  , ( "subject", showJSON importAliases subject )
                  , ( "branches"
                    , JSArray $ map
                        (\(Commented _ (A _ pat) _, (_, body)) ->
                           makeObj
                               [ ("pattern", showJSON importAliases pat)
                               , ("body", showJSON importAliases body)
                               ]
                        )
                        branches
                    )
                  ]

          _ ->
              JSString $ toJSString "TODO: Expr"


variableReference :: Map.Map UppercaseIdentifier [UppercaseIdentifier] -> Region.Region -> String -> JSValue
variableReference x region name =
    makeObj
        [ type_ "VariableReference"
        , ( "name" , JSString $ toJSString name )
        , sourceLocation x region
        ]


sourceLocation :: Map.Map UppercaseIdentifier [UppercaseIdentifier] -> Region.Region -> (String, JSValue)
sourceLocation x region =
    ( "sourceLocation", showJSON x region )


instance ToJSON LetDeclaration where
  showJSON importAliases letDeclaration =
      case letDeclaration of
          LetDefinition (A _ (VarPattern (LowercaseIdentifier var))) [] _ expr ->
              makeObj
                  [ type_ "Definition"
                  , ("name" , JSString $ toJSString var)
                  , ("expression" , showJSON importAliases expr)
                  ]

          _ ->
              JSString $ toJSString $ "TODO: LetDeclaration (" ++ show letDeclaration ++ ")"


instance ToJSON IntRepresentation where
    showJSON _ DecimalInt = JSString $ toJSString $ "DecimalInt"
    showJSON _ HexadecimalInt = JSString $ toJSString $ "HexadecimalInt"


instance ToJSON Pattern' where
  showJSON _ pattern' =
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

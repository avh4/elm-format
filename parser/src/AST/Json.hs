{-# LANGUAGE OverloadedStrings #-}
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


instance ToJSON Decl where
  showJSON importAliases (Decl (A _ (Definition (A _ (VarPattern (LowercaseIdentifier var))) _ _ (A _ expr)))) =
    makeObj
      [ type_ "Definition"
      , ("name" , JSString $ toJSString var)
      , ("expression" , showJSON importAliases expr)
      ]
  showJSON _ _ = JSString $ toJSString "TODO: Decl"


instance ToJSON Expr' where
  showJSON importAliases expr =
      case expr of
          Unit _ ->
              makeObj [ type_ "UnitLiteral" ]

          VarExpr (VarRef [] (LowercaseIdentifier var)) ->
              makeObj
                  [ type_ "VariableReference"
                  , ("name" , JSString $ toJSString var)
                  ]

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
                  ]

          App (A _ expr) args _ ->
              makeObj
                  [ type_ "FunctionApplication"
                  , ("function", showJSON importAliases expr)
                  , ("arguments", JSArray $ showJSON importAliases <$> map (\(_ , A _ arg) -> arg) args)
                  ]

          Binops (A _ first) rest _ ->
              makeObj
                  [ type_ "BinaryOperatorList"
                  , ("first", showJSON importAliases first)
                  , ("operations"
                    , JSArray $ map
                        (\(_, OpRef (SymbolIdentifier ref), _, A _ expr) ->
                           makeObj
                               [ ("operator", JSString $ toJSString ref)
                               , ("term", showJSON importAliases expr)
                               ]
                        )
                        rest
                    )
                  ]

          Unary Negative (A _ expr) ->
            makeObj
                [ type_ "UnaryOperator"
                , ("operator", JSString $ toJSString "-")
                , ("term", showJSON importAliases expr)
                ]

          Parens (Commented _ (A _ expr) _) ->
              showJSON importAliases expr

          ExplicitList terms _ _ ->
              makeObj
                  [ type_ "ListLiteral"
                  , ("terms", JSArray $ fmap (showJSON importAliases) (map (\(_, (_, (A _ term, _))) -> term) terms))
                  ]

          AST.Expression.Tuple exprs _ ->
              makeObj
                  [ type_ "TupleLiteral"
                  , ("terms", JSArray $ fmap (showJSON importAliases) (map (\(Commented _ (A _ expr) _) -> expr) exprs))
                  ]

          AST.Expression.Record base fields _ _ ->
              let
                  fieldsJSON =
                      ( "fields"
                      , makeObj $ map
                          (\(_, (_, (Pair (LowercaseIdentifier key, _) (_, A _ value) _, _))) ->
                             (key, showJSON importAliases value)
                          )
                          fields
                      )
              in
              case base of
                  Just (Commented _ (LowercaseIdentifier identifier) _) ->
                      makeObj
                          [ type_ "RecordUpdate"
                          , ("base", JSString $ toJSString identifier)
                          , fieldsJSON
                          ]

                  Nothing ->
                      makeObj
                          [ type_ "RecordLiteral"
                          , fieldsJSON
                          ]

          Lambda parameters _ (A _ body) _ ->
              makeObj
                  [ type_ "AnonymousFunction"
                  , ("parameters", JSArray $ map (\(_, A _ pat) -> showJSON importAliases pat) parameters)
                  , ("body", showJSON importAliases body)
                  ]

          _ ->
              JSString $ toJSString "TODO: Expr"


instance ToJSON Pattern' where
  showJSON _ pattern' =
      JSString $ toJSString $ "TODO: Pattern (" ++ show pattern' ++ ")"


type_ :: String -> (String, JSValue)
type_ t =
    ("type", JSString $ toJSString t)

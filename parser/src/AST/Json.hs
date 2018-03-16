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
      [ ("type" , JSString $ toJSString "Definition")
      , ("name" , JSString $ toJSString var)
      , ("expression" , showJSON importAliases expr)
      ]
  showJSON _ _ = JSString $ toJSString "TODO: Decl"


instance ToJSON Expr' where
  showJSON importAliases expr =
      case expr of
          Unit _ ->
              makeObj [ ("type", JSString $ toJSString "UnitLiteral") ]

          VarExpr (VarRef [] (LowercaseIdentifier var)) ->
              makeObj
                  [ ("type" , JSString $ toJSString "VariableReference")
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
                  [ ("type" , JSString $ toJSString "ExternalReference")
                  , ("module"
                    , JSString $ toJSString $ List.intercalate "." $ map (\(UppercaseIdentifier v) -> v) normalizedNamespace)
                  , ("identifier", JSString $ toJSString var)
                  ]

          App (A _ expr) args _ ->
              makeObj
                  [ ("type" , JSString $ toJSString "FunctionApplication")
                  , ("function", showJSON importAliases expr)
                  , ("arguments", JSArray $ showJSON importAliases <$> map (\(_ , A _ arg) -> arg) args)
                  ]

          Binops (A _ first) rest _ ->
              makeObj
                  [ ("type", JSString $ toJSString "BinaryOperatorList")
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
                [ ("type", JSString $ toJSString "UnaryOperator")
                , ("operator", JSString $ toJSString "-")
                , ("term", showJSON importAliases expr)
                ]

          Parens (Commented _ (A _ expr) _) ->
              showJSON importAliases expr

          ExplicitList terms _ _ ->
              makeObj
                  [ ("type", JSString $ toJSString "ListLiteral")
                  , ("terms", JSArray $ fmap (showJSON importAliases) (map (\(_, (_, (A _ term, _))) -> term) terms))
                  ]

          AST.Expression.Tuple exprs _ ->
              makeObj
                  [ ("type", JSString $ toJSString "TupleLiteral")
                  , ("terms", JSArray $ fmap (showJSON importAliases) (map (\(Commented _ (A _ expr) _) -> expr) exprs))
                  ]

          AST.Expression.Record Nothing fields _ _ ->
              makeObj
                  [ ( "type", JSString $ toJSString "RecordLiteral" )
                  , ( "fields"
                    , makeObj $ map
                        (\(_, (_, (Pair (LowercaseIdentifier key, _) (_, A _ value) _, _))) ->
                           (key, showJSON importAliases value)
                        )
                        fields
                    )
                  ]

          _ ->
              JSString $ toJSString "TODO: Expr"

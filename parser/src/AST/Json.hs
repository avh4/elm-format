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
  showJSON importAliases (Decl (A _ (Definition (A _ (VarPattern (LowercaseIdentifier var))) params _ (A _ expr)))) =
    makeObj
      [ ("type" , JSString $ toJSString "Definition")
      , ("name" , JSString $ toJSString var)
      , ("expression" , showJSON importAliases expr)
      ]
  showJSON _ _ = JSString $ toJSString "TODO: Decl"


instance ToJSON Expr' where
  showJSON _ (VarExpr (VarRef [] (LowercaseIdentifier var))) =
    makeObj
      [ ("type" , JSString $ toJSString "VariableReference")
      , ("name" , JSString $ toJSString var)
      ]
  showJSON importAliases (VarExpr (VarRef namespace (LowercaseIdentifier var))) =
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
  showJSON importAliases (App (A _ expr) _ _) =
    makeObj
      [ ("type" , JSString $ toJSString "FunctionApplication")
      , ("function", showJSON importAliases expr)
      ]
  showJSON _ _ = JSString $ toJSString "TODO: Expr"

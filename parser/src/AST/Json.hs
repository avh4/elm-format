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

class ToJSON a where
  showJSON :: a -> JSValue


instance ToJSON Module where
  showJSON (Module _ _ _ _ body) =
    makeObj [ ("body" , JSArray $ fmap showJSON body) ]


instance ToJSON Decl where
  showJSON (Decl (A _ (Definition (A _ (VarPattern (LowercaseIdentifier var))) params _ (A _ expr)))) =
    makeObj
      [ ("type" , JSString $ toJSString "Definition")
      , ("name" , JSString $ toJSString var)
      , ("expression" , showJSON expr)
      ]
  showJSON _ = JSString $ toJSString "TODO: Decl"


instance ToJSON Expr' where
  showJSON (VarExpr (VarRef [] (LowercaseIdentifier var))) =
    makeObj
      [ ("type" , JSString $ toJSString "VariableReference")
      , ("name" , JSString $ toJSString var)
      ]
  showJSON (VarExpr (VarRef namespace (LowercaseIdentifier var))) =
    makeObj
      [ ("type" , JSString $ toJSString "ExternalReference")
      , ("module", JSString $ toJSString $ List.intercalate "." $ map (\(UppercaseIdentifier v) -> v) namespace)
      , ("identifier", JSString $ toJSString var)
      ]
  showJSON (App (A _ expr) _ _) =
    makeObj
      [ ("type" , JSString $ toJSString "FunctionApplication")
      , ("function", showJSON expr)
      ]
  showJSON _ = JSString $ toJSString "TODO: Expr"

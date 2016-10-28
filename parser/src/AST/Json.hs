{-# LANGUAGE OverloadedStrings #-}
module AST.Json where

import AST.Declaration
import AST.Expression
import AST.Module
import AST.Pattern
import AST.Variable
import AST.V0_16
import Data.Aeson
import Data.Monoid ((<>))
import Reporting.Annotation hiding (map)

import qualified Data.List as List


instance ToJSON Module where
  toJSON (Module _ _ _ _ body) =
    object [ "body" .= body ]
  toEncoding (Module _ _ _ _ body) =
    pairs ("body" .= body)


instance ToJSON Decl where
  toJSON (Decl (A _ (Definition (A _ (VarPattern (LowercaseIdentifier var))) params _ (A _ expr)))) =
    object
      [ "type" .= ("Definition" :: String)
      , "name" .= var
      , "expression" .= expr
      ]
  toJSON _ = String "TODO: Decl"


instance ToJSON Expr' where
  toJSON (VarExpr (VarRef [] (LowercaseIdentifier var))) =
    object
      [ "type" .= ("VariableReference" :: String)
      , "name" .= var
      ]
  toJSON (VarExpr (VarRef namespace (LowercaseIdentifier var))) =
    object
      [ "type" .= ("VariableReference" :: String)
      , "name" .= ((List.intercalate "." $ map (\(UppercaseIdentifier v) -> v) namespace) ++ "." ++ var)
      ]
  toJSON _ = String "TODO: Expr"

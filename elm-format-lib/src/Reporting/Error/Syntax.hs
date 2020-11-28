{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Syntax where

import AST.V0_16
import qualified Text.Parsec.Error as Parsec


data Error
    = Parse [Parsec.Message]
    | InfixDuplicate (Ref [UppercaseIdentifier])
    | TypeWithoutDefinition String
    | PortWithoutAnnotation String
    | UnexpectedPort
    | DuplicateValueDeclaration String
    | DuplicateTypeDeclaration String
    | DuplicateDefinition String
    deriving (Eq, Show)


instance Show Parsec.Message where
    show _ = "<Text.Parsec.Error.Message>"


-- TAGGING PARSE ERRORS

newline :: String
newline = "NEWLINE"

freshLine :: String
freshLine = "FRESH_LINE"

whitespace :: String
whitespace = "WHITESPACE"

keyword :: String -> String
keyword kwd =
  "KEYWORD=" ++ kwd

{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Syntax where

import AST.V0_16
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Text.Parsec.Error as Parsec

import qualified AST.Variable as Var
import qualified Reporting.Report as Report


data Error
    = Parse [Parsec.Message]
    | InfixDuplicate Var.Ref
    | TypeWithoutDefinition String
    | PortWithoutAnnotation String
    | UnexpectedPort
    | DuplicateValueDeclaration String
    | DuplicateTypeDeclaration String
    | DuplicateDefinition String
    deriving (Eq)


instance Show Error where
    show e =
        show $ toReport e


-- TO REPORT

toReport :: Error -> Report.Report
toReport err =
  case err of
    Parse messages ->
        parseErrorReport messages

    InfixDuplicate op ->
        Report.simple
          "INFIX OVERLAP"
          ("The infix declarations for " ++ operator ++ " must be removed.")
          ("The precedence and associativity can only be set in one place, and\n"
           ++ "this information has already been set somewhere else."
          )
      where
        operator =
            case op of
              Var.VarRef _namespace (LowercaseIdentifier name) -> "`" ++ name ++ "`"
              Var.TagRef _namespace (UppercaseIdentifier name) -> "`" ++ name ++ "`"
              Var.OpRef (SymbolIdentifier name) -> "(" ++ name ++ ")"

    TypeWithoutDefinition valueName ->
        Report.simple
          "MISSING DEFINITION"
          ("There is a type annotation for `" ++ valueName ++ "` but there"
            ++ " is no corresponding definition!"
          )
          ("Directly below the type annotation, put a definition like:\n\n"
            ++ "    " ++ valueName ++ " = 42"
          )

    PortWithoutAnnotation portName ->
        Report.simple
          "PORT ERROR"
          ("Port `" ++ portName ++ "` does not have a type annotation!")
          ("Directly above the port definition, I need something like this:\n\n"
            ++ "    port " ++ portName ++ " : Signal Int"
          )


    UnexpectedPort ->
        Report.simple
          "PORT ERROR"
          "This module has ports, but ports can only appear in the main module."
          ( "Ports in library code would create hidden dependencies where importing a\n"
            ++ "module could bring in constraints not captured in the public API. Furthermore,\n"
            ++ "if the module is imported twice, do we send values out the port twice?"
          )

    DuplicateValueDeclaration name ->
        Report.simple
          "DUPLICATE DEFINITION"
          ("Naming multiple top-level values `" ++ name ++ "` makes things\n"
            ++ "ambiguous. When you say `" ++ name ++ "` which one do you want?"
          )
          ("Find all the top-level values named `" ++ name ++ "` and\n"
            ++ "do some renaming. Make sure the names are distinct!"
          )

    DuplicateTypeDeclaration name ->
        Report.simple
          "DUPLICATE DEFINITION"
          ("Naming multiple types `" ++ name ++ "` makes things ambiguous\n"
            ++ "When you say `" ++ name ++ "` which one do you want?"
          )
          ("Find all the types named `" ++ name ++ "` and\n"
            ++ "do some renaming. Make sure the names are distinct!"
          )

    DuplicateDefinition name ->
        Report.simple
          "DUPLICATE DEFINITION"
          ("Naming multiple values `" ++ name ++ "` in a single let-expression makes\n"
            ++ "things ambiguous. When you say `" ++ name ++ "` which one do you want?"
          )
          ("Find all the values named `" ++ name ++ "` in this let-expression and\n"
            ++ "do some renaming. Make sure the names are distinct!"
          )


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

unkeyword :: String -> Maybe String
unkeyword message =
  if List.isPrefixOf "KEYWORD=" message
    then Just (drop (length "KEYWORD=") message)
    else Nothing


-- REPORTING PARSE ERRORS

parseErrorReport :: [Parsec.Message] -> Report.Report
parseErrorReport messages =
  let
    addMsg message hint =
      case message of
        Parsec.SysUnExpect _msg ->
            hint

        Parsec.UnExpect _msg ->
            hint

        Parsec.Expect msg ->
          let
            msg' =
              if msg `elem` [whitespace, newline, freshLine]
                then "whitespace"
                else msg
          in
            hint { _expected = Set.insert msg' (_expected hint) }

        Parsec.Message msg ->
            hint { _messages = msg : _messages hint }

    (ParseHint msgs expects) =
      foldr addMsg emptyHint messages

    preHint =
      case msgs of
        [msg] ->
            case unkeyword msg of
              Just kwd ->
                  "It looks like the keyword `" ++ kwd ++ "` is being used as a variable.\n"
                  ++ "Try renaming it to something else."
              Nothing ->
                  msg

        _ -> "I ran into something unexpected when parsing your code!"

    postHint =
      if Set.null expects
        then ""
        else
          "I am looking for one of the following things:\n"
          ++ concatMap ("\n    "++) (Set.toList expects)
  in
    Report.simple "SYNTAX PROBLEM" preHint postHint


data ParseHint = ParseHint
    { _messages :: [String]
    , _expected :: Set.Set String
    }
    deriving (Show)


emptyHint :: ParseHint
emptyHint =
  ParseHint [] Set.empty

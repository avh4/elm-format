{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Type where

import Text.Parsec ((<|>), (<?>), char, many1, string, try, optionMaybe)

import Parse.Helpers
import qualified Reporting.Annotation as A
import qualified AST.V0_16 as AST
import Parse.IParser
import Parse.Common
import Data.Maybe (maybeToList)


tvar :: IParser AST.Type
tvar =
  addLocation
    (AST.TypeVariable <$> lowVar <?> "a type variable")


tuple :: IParser AST.Type
tuple =
  addLocation $
  do  types <- parens'' (withEol expr)
      case types of
        Left comments ->
            return $ AST.UnitType comments
        Right [] ->
            return $ AST.UnitType []
        Right [AST.Commented [] (t, Nothing) []] ->
            return $ A.drop t
        Right [AST.Commented pre (t, eol) post] ->
            return $ AST.TypeParens (AST.Commented pre t (maybeToList (fmap AST.LineComment eol) ++ post))
        Right types' ->
            return $ AST.TupleType types'


record :: IParser AST.Type
record =
    addLocation $ brackets' $ checkMultiline $
        do
            base <- optionMaybe $ try (commented lowVar <* string "|")
            (fields, trailing) <- sectionedGroup (pair lowVar lenientHasType expr)
            return $ AST.RecordType base fields trailing


capTypeVar :: IParser [AST.UppercaseIdentifier]
capTypeVar =
    dotSep1 capVar


constructor0 :: IParser AST.TypeConstructor
constructor0 =
  do  name <- capTypeVar
      return (AST.NamedConstructor name)


constructor0' :: IParser AST.Type
constructor0' =
    addLocation $
    do  ctor <- constructor0
        return (AST.TypeConstruction ctor [])


term :: IParser AST.Type
term =
  tuple <|> record <|> tvar <|> constructor0'


tupleCtor :: IParser AST.TypeConstructor
tupleCtor =
    do  ctor <- parens' (many1 (char ','))
        return (AST.TupleConstructor (length ctor + 1))


app :: IParser AST.Type
app =
  addLocation $
  do  f <- constructor0 <|> try tupleCtor <?> "a type constructor"
      args <- spacePrefix term
      return $ AST.TypeConstruction f args


expr :: IParser AST.Type
expr =
  do
    result <- separated rightArrow (app <|> term)
    return $
      case result of
        Left t ->
          t
        Right (region, first, rest, multiline) ->
          A.A region $ AST.FunctionType first rest (AST.ForceMultiline multiline)


constructor :: IParser ([AST.UppercaseIdentifier], [(AST.Comments, AST.Type)])
constructor =
  (,) <$> (capTypeVar <?> "another type constructor")
      <*> spacePrefix term


tag :: IParser (AST.UppercaseIdentifier, [(AST.Comments, AST.Type)])
tag =
  (,) <$> (capVar <?> "another type constructor")
      <*> spacePrefix term

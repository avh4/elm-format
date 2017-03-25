{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Type where

import Text.Parsec ((<|>), (<?>), char, many1, string, try, optionMaybe)

import Parse.Helpers
import qualified Reporting.Annotation as A
import AST.V0_16
import Parse.IParser
import Parse.Whitespace
import Parse.Common


tvar :: IParser Type
tvar =
  addLocation
    (TypeVariable <$> lowVar <?> "a type variable")


tuple :: IParser Type
tuple =
  addLocation $
  do  types <- parens'' expr
      case types of
        Left comments ->
            return $ UnitType comments
        Right [] ->
            return $ UnitType []
        Right [Commented [] t []] ->
            return $ A.drop t
        Right [t] ->
            return $ TypeParens t
        Right types' ->
            return $ TupleType types'


record :: IParser Type
record =
    addLocation $ brackets' $ checkMultiline $
        do
            base <- optionMaybe $ try (commented lowVar <* string "|")
            (fields, trailing) <- sectionedGroup (pair lowVar lenientHasType expr)
            return $ RecordType base fields trailing


capTypeVar :: IParser [UppercaseIdentifier]
capTypeVar =
    dotSep1 capVar


constructor0 :: IParser TypeConstructor
constructor0 =
  do  name <- capTypeVar
      return (NamedConstructor name)


constructor0' :: IParser Type
constructor0' =
    addLocation $
    do  ctor <- constructor0
        return (TypeConstruction ctor [])


term :: IParser Type
term =
  tuple <|> record <|> tvar <|> constructor0'


tupleCtor :: IParser TypeConstructor
tupleCtor =
    do  ctor <- parens' (many1 (char ','))
        return (TupleConstructor (length ctor + 1))


app :: IParser Type
app =
  addLocation $
  do  f <- constructor0 <|> try tupleCtor <?> "a type constructor"
      args <- spacePrefix term
      return $ TypeConstruction f args


expr :: IParser Type
expr =
  do
    result <- separated rightArrow (app <|> term)
    return $
      case result of
        Left t ->
          t
        Right (region, first, rest, multiline) ->
          A.A region $ FunctionType first rest (ForceMultiline multiline)


constructor :: IParser ([UppercaseIdentifier], [(Comments, Type)])
constructor =
  (,) <$> (capTypeVar <?> "another type constructor")
      <*> spacePrefix term


tag :: IParser (UppercaseIdentifier, [(Comments, Type)])
tag =
  (,) <$> (capVar <?> "another type constructor")
      <*> spacePrefix term

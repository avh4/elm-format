{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Type where

import Text.Parsec ((<|>), (<?>), char, many1, string, try, optionMaybe)

import Parse.Helpers
import qualified Reporting.Annotation as A
import qualified AST.V0_16 as AST
import ElmVersion
import Parse.IParser
import Parse.Common
import Data.Maybe (maybeToList)


tvar :: ElmVersion -> IParser AST.Type
tvar elmVersion =
  addLocation
    (AST.TypeVariable <$> lowVar elmVersion <?> "a type variable")


tuple :: ElmVersion -> IParser AST.Type
tuple elmVersion =
  addLocation $
  do  types <- parens'' (withEol $ expr elmVersion)
      case types of
        Left comments ->
            return $ AST.UnitType comments
        Right [] ->
            return $ AST.UnitType []
        Right [AST.Commented [] (AST.WithEol t Nothing) []] ->
            return $ A.drop t
        Right [AST.Commented pre (AST.WithEol t eol) post] ->
            return $ AST.TypeParens (AST.Commented pre t (maybeToList (fmap AST.LineComment eol) ++ post))
        Right types' ->
            return $ AST.TupleType types'


record :: ElmVersion -> IParser AST.Type
record elmVersion =
    addLocation $ brackets' $ checkMultiline $
        do
            base <- optionMaybe $ try (commented (lowVar elmVersion) <* string "|")
            (fields, trailing) <- sectionedGroup (pair (lowVar elmVersion) lenientHasType (expr elmVersion))
            return $ AST.RecordType base fields trailing


capTypeVar :: ElmVersion -> IParser [AST.UppercaseIdentifier]
capTypeVar elmVersion =
    dotSep1 (capVar elmVersion)


constructor0 :: ElmVersion -> IParser AST.TypeConstructor
constructor0 elmVersion =
  do  name <- capTypeVar elmVersion
      case reverse name of
        [] -> error "Impossible empty TypeConstructor name"
        last:rest ->
            return (AST.NamedConstructor (reverse rest) last)


constructor0' :: ElmVersion -> IParser AST.Type
constructor0' elmVersion =
    addLocation $
    do  ctor <- constructor0 elmVersion
        return (AST.TypeConstruction ctor [])


term :: ElmVersion -> IParser AST.Type
term elmVersion =
  tuple elmVersion <|> record elmVersion <|> tvar elmVersion <|> constructor0' elmVersion


tupleCtor :: IParser AST.TypeConstructor
tupleCtor =
    do  ctor <- parens' (many1 (char ','))
        return (AST.TupleConstructor (length ctor + 1))


app :: ElmVersion -> IParser AST.Type
app elmVersion =
  addLocation $
  do  f <- constructor0 elmVersion <|> try tupleCtor <?> "a type constructor"
      args <- spacePrefix (term elmVersion)
      return $ AST.TypeConstruction f args


expr :: ElmVersion -> IParser AST.Type
expr elmVersion =
  do
    result <- separated rightArrow (app elmVersion <|> term elmVersion)
    return $
      case result of
        Left t ->
          t
        Right (region, first, rest, multiline) ->
          A.A region $ AST.FunctionType first rest (AST.ForceMultiline multiline)


constructor :: ElmVersion -> IParser ([AST.UppercaseIdentifier], [(AST.Comments, AST.Type)])
constructor elmVersion =
  (,) <$> (capTypeVar elmVersion<?> "another type constructor")
      <*> spacePrefix (term elmVersion)


tag :: ElmVersion -> IParser (AST.UppercaseIdentifier, [(AST.Comments, AST.Type)])
tag elmVersion =
  (,) <$> (capVar elmVersion <?> "another type constructor")
      <*> spacePrefix (term elmVersion)

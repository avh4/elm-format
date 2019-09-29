{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Type where

import Text.Parsec ((<|>), (<?>), char, many1, string, try, optionMaybe)

import Parse.Helpers
import qualified Reporting.Annotation as A
import AST.V0_16
import ElmVersion
import Parse.IParser
import Parse.Common
import Data.Maybe (maybeToList)


tvar :: ElmVersion -> IParser (Type ns)
tvar elmVersion =
  addLocation
    (TypeVariable <$> lowVar elmVersion <?> "a type variable")


tuple :: ElmVersion -> IParser (Type [UppercaseIdentifier])
tuple elmVersion =
  addLocation $
  do  types <- parens'' (withEol $ expr elmVersion)
      case types of
        Left comments ->
            return $ UnitType comments
        Right [] ->
            return $ UnitType []
        Right [Commented [] (WithEol t Nothing) []] ->
            return $ A.drop t
        Right [Commented pre (WithEol t eol) post] ->
            return $ TypeParens (Commented pre t (maybeToList (fmap LineComment eol) ++ post))
        Right types' ->
            return $ TupleType types'


record :: ElmVersion -> IParser (Type [UppercaseIdentifier])
record elmVersion =
    addLocation $ brackets' $ checkMultiline $
        do
            base <- optionMaybe $ try (commented (lowVar elmVersion) <* string "|")
            (fields, trailing) <- sectionedGroup (pair (lowVar elmVersion) lenientHasType (expr elmVersion))
            return $ RecordType base fields trailing


capTypeVar :: ElmVersion -> IParser [UppercaseIdentifier]
capTypeVar elmVersion =
    dotSep1 (capVar elmVersion)


constructor0 :: ElmVersion -> IParser (TypeConstructor [UppercaseIdentifier])
constructor0 elmVersion =
  do  name <- capTypeVar elmVersion
      case reverse name of
        [] -> error "Impossible empty TypeConstructor name"
        last:rest ->
            return (NamedConstructor (reverse rest) last)


constructor0' :: ElmVersion -> IParser (Type [UppercaseIdentifier])
constructor0' elmVersion =
    addLocation $
    do  ctor <- constructor0 elmVersion
        return (TypeConstruction ctor [])


term :: ElmVersion -> IParser (Type [UppercaseIdentifier])
term elmVersion =
  tuple elmVersion <|> record elmVersion <|> tvar elmVersion <|> constructor0' elmVersion


tupleCtor :: IParser (TypeConstructor ns)
tupleCtor =
    do  ctor <- parens' (many1 (char ','))
        return (TupleConstructor (length ctor + 1))


app :: ElmVersion -> IParser (Type [UppercaseIdentifier])
app elmVersion =
  addLocation $
  do  f <- constructor0 elmVersion <|> try tupleCtor <?> "a type constructor"
      args <- spacePrefix (term elmVersion)
      return $ TypeConstruction f args


expr :: ElmVersion -> IParser (Type [UppercaseIdentifier])
expr elmVersion =
  do
    result <- separated rightArrow (app elmVersion <|> term elmVersion)
    return $
      case result of
        Left t ->
          t
        Right (region, first, rest, multiline) ->
          A.A region $ FunctionType first rest (ForceMultiline multiline)


constructor :: ElmVersion -> IParser ([UppercaseIdentifier], [(Comments, Type [UppercaseIdentifier])])
constructor elmVersion =
  (,) <$> (capTypeVar elmVersion<?> "another type constructor")
      <*> spacePrefix (term elmVersion)


tag :: ElmVersion -> IParser (UppercaseIdentifier, [(Comments, Type [UppercaseIdentifier])])
tag elmVersion =
  (,) <$> (capVar elmVersion <?> "another type constructor")
      <*> spacePrefix (term elmVersion)

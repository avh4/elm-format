{-# LANGUAGE DataKinds #-}
module Parse.Type where

import Parse.ParsecAdapter ((<|>), (<?>), char, many1, string, try, optionMaybe)

import Parse.Helpers
import Reporting.Annotation (Located)
import qualified Reporting.Annotation as A
import AST.V0_16
import AST.Structure
import Data.Coapplicative
import qualified Data.Indexed as I
import ElmVersion
import Parse.IParser
import Parse.Common


tvar :: ElmVersion -> IParser (FixAST Located typeRef ctorRef varRef 'TypeNK)
tvar elmVersion =
  fmap I.Fix $ addLocation
    (TypeVariable <$> lowVar elmVersion <?> "a type variable")


tuple :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'TypeNK)
tuple elmVersion =
  fmap I.Fix $ addLocation $ checkMultiline $
  do  types <- parens'' (withEol $ expr elmVersion)
      return $
          case types of
              Left comments ->
                  \_ -> UnitType comments
              Right [] ->
                  \_ -> UnitType []
              Right [C ([], []) (C Nothing t)] ->
                  \_ -> extract $ I.unFix t
              Right [C (pre, post) (C eol t)] ->
                  \_ -> TypeParens $ C (pre, eolToComment eol ++ post) t
              Right types' ->
                  TupleType $ fmap (\(C (pre, post) (C eol t)) -> C (pre, post, eol) t) types'


record :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'TypeNK)
record elmVersion =
    fmap I.Fix $ addLocation $ brackets' $ checkMultiline $
        do
            base' <- optionMaybe $ try (commented (lowVar elmVersion) <* string "|")
            (fields', trailing) <- sectionedGroup (pair (lowVar elmVersion) lenientHasType (expr elmVersion))
            return $ RecordType base' fields' trailing


capTypeVar :: ElmVersion -> IParser [UppercaseIdentifier]
capTypeVar elmVersion =
    dotSep1 (capVar elmVersion)


constructor0 :: ElmVersion -> IParser (TypeConstructor ([UppercaseIdentifier], UppercaseIdentifier))
constructor0 elmVersion =
  do  name <- capTypeVar elmVersion
      case reverse name of
        [] -> error "Impossible empty TypeConstructor name"
        last':rest' ->
            return (NamedConstructor (reverse rest', last'))


constructor0' :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'TypeNK)
constructor0' elmVersion =
    fmap I.Fix $ addLocation $ checkMultiline $
    do  ctor <- constructor0 elmVersion
        return (TypeConstruction ctor [])


term :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'TypeNK)
term elmVersion =
  tuple elmVersion <|> record elmVersion <|> tvar elmVersion <|> constructor0' elmVersion


tupleCtor :: IParser (TypeConstructor ns)
tupleCtor =
    do  ctor <- parens' (many1 (char ','))
        return (TupleConstructor (length ctor + 1))


app :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'TypeNK)
app elmVersion =
  fmap I.Fix $ addLocation $ checkMultiline $
  do  f <- constructor0 elmVersion <|> try tupleCtor <?> "a type constructor"
      args <- spacePrefix (term elmVersion)
      return $ TypeConstruction f args


expr :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'TypeNK)
expr elmVersion =
  do
    result <- separated rightArrow (app elmVersion <|> term elmVersion)
    return $
      case result of
        Left t ->
          t
        Right (region, first', rest', multiline) ->
          I.Fix $ A.At region $ FunctionType first' rest' (ForceMultiline multiline)


-- TODO: can this be removed?  (tag is the new name?)
constructor :: ElmVersion -> IParser ([UppercaseIdentifier], [C1 before (ASTNS Located [UppercaseIdentifier] 'TypeNK)])
constructor elmVersion =
  (,) <$> (capTypeVar elmVersion<?> "another type constructor")
      <*> spacePrefix (term elmVersion)


tag :: ElmVersion -> IParser (NameWithArgs UppercaseIdentifier (ASTNS Located [UppercaseIdentifier] 'TypeNK))
tag elmVersion =
  NameWithArgs
      <$> (capVar elmVersion <?> "another type constructor")
      <*> spacePrefix (term elmVersion)

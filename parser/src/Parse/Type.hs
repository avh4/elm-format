{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Type where

import Data.List (intercalate)
import Text.Parsec ((<|>), (<?>), char, many1, optionMaybe, string, try)

import Parse.Helpers
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import AST.V0_16


tvar :: IParser Type
tvar =
  addLocation
    (TypeVariable <$> lowVar <?> "a type variable")


tuple :: IParser Type
tuple =
  do  (start, types, end) <- located (parens $ ((\f a b _ -> f a b) <$> commaSep (const . const <$> expr))) -- TODO: use comments
      case types of
        [] -> return $ A.A (R.Region start end) $ UnitType
        [t] -> return t
        _   -> return $ A.A (R.Region start end) $ TupleType types


record :: IParser Type
record =
  addLocation $
  do  char '{'
      pushNewlineContext
      whitespace -- TODO: use comments
      (ext, fields) <- extended <|> normal
      dumbWhitespace -- TODO: use comments
      char '}'
      sawNewline <- popNewlineContext
      return $ RecordType ext (fields [] []) sawNewline -- TODO: pass comments
  where
    normal =
      do  (\fields -> (Nothing, fields) ) <$> commaSep field

    -- extended record types require at least one field
    extended =
      do  ext <- try (lowVar <* (whitespace >> string "|")) -- TODO: use comments
          whitespace -- TODO: use comments
          fields <- commaSep1 field
          return (Just ext, fields)

    field =
      do  pushNewlineContext
          lbl <- rLabel
          whitespace >> hasType >> whitespace -- TODO: use comments
          val <- expr
          sawNewline <- popNewlineContext
          return $ \_ _ -> (lbl, val, sawNewline) -- TODO: use comments


capTypeVar :: IParser String
capTypeVar =
  intercalate "." <$> dotSep1 capVar


constructor0 :: IParser TypeConstructor
constructor0 =
  do  name <- capTypeVar
      return (NamedConstructor name)


constructor0' :: IParser Type
constructor0' =
    addLocation $
    do  ctor <- capTypeVar
        return (TypeVariable ctor)


term :: IParser Type
term =
  tuple <|> record <|> tvar <|> constructor0'


tupleCtor :: IParser TypeConstructor
tupleCtor =
    do  ctor <- parens' (many1 (char ','))
        return (TupleConstructor (length ctor + 1))


app :: IParser Type
app =
  do  start <- getMyPosition
      f <- constructor0 <|> try tupleCtor <?> "a type constructor"
      args <- spacePrefix term
      end <- getMyPosition
      return (A.A (R.Region start end) (TypeConstruction f args))


expr :: IParser Type
expr =
  do  start <- getMyPosition
      t1 <- app <|> term
      arr <- optionMaybe $ try (whitespace >> rightArrow) -- TODO: use comments
      case arr of
        Nothing ->
            return t1
        Just _ ->
            do  whitespace -- TODO: use comments
                t2 <- expr
                end <- getMyPosition
                case A.drop t2 of
                    FunctionType t2' ts ->
                        return (A.A (R.Region start end) (FunctionType t1 (t2':ts)))
                    _ ->
                        return (A.A (R.Region start end) (FunctionType t1 [t2]))


constructor :: IParser (String, [Type])
constructor =
  (,) <$> (capTypeVar <?> "another type constructor")
      <*> (map (\(_,v) -> v) <$> spacePrefix term) -- TODO: use comments

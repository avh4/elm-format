{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Type where

import Data.List (intercalate)
import Text.Parsec ((<|>), (<?>), char, many1, optionMaybe, string, try)

import qualified AST.Type as Type
import qualified AST.Variable as Var
import Parse.Helpers
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


tvar :: IParser Type.Type
tvar =
  addLocation
    (Type.RVar <$> lowVar <?> "a type variable")


tuple :: IParser Type.Type
tuple =
  do  (start, types, end) <- located (parens (fmap const $ commaSep expr))
      case types of
        [t] -> return t
        _   -> return (Type.tuple (R.Region start end) types)


record :: IParser Type.Type
record =
  addLocation $
  do  char '{'
      pushNewlineContext
      whitespace
      (ext, fields) <- extended <|> normal
      dumbWhitespace
      char '}'
      sawNewline <- popNewlineContext
      return $ Type.RRecord ext fields sawNewline
  where
    normal =
      do  (\fields -> (Nothing, fields) ) <$> commaSep field

    -- extended record types require at least one field
    extended =
      do  ext <- try (addLocation lowVar <* (whitespace >> string "|"))
          whitespace
          fields <- commaSep1 field
          return ((Just (A.map Type.RVar ext)), fields)

    field =
      do  pushNewlineContext
          lbl <- rLabel
          whitespace >> hasType >> whitespace
          val <- expr
          sawNewline <- popNewlineContext
          return (lbl, val, sawNewline)


capTypeVar :: IParser String
capTypeVar =
  intercalate "." <$> dotSep1 capVar


constructor0 :: IParser Type.Type
constructor0 =
  addLocation $
  do  name <- capTypeVar
      return (Type.RType (Var.VarRef name))


term :: IParser Type.Type
term =
  tuple <|> record <|> tvar <|> constructor0


app :: IParser Type.Type
app =
  do  start <- getMyPosition
      f <- constructor0 <|> try tupleCtor <?> "a type constructor"
      args <- spacePrefix term
      end <- getMyPosition
      case args of
        [] -> return f
        _  -> return (A.A (R.Region start end) (Type.RApp f args))
  where
    tupleCtor =
      addLocation $
      do  ctor <- parens (fmap const $ many1 (char ','))
          return (Type.RType (Var.VarRef ctor))


expr :: IParser Type.Type
expr =
  do  start <- getMyPosition
      t1 <- app <|> term
      arr <- optionMaybe $ try (whitespace >> rightArrow)
      case arr of
        Nothing ->
            return t1
        Just _ ->
            do  whitespace
                t2 <- expr
                end <- getMyPosition
                return (A.A (R.Region start end) (Type.RLambda t1 t2))


constructor :: IParser (String, [Type.Type])
constructor =
  (,) <$> (capTypeVar <?> "another type constructor")
      <*> spacePrefix term

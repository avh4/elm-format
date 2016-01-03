{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Type where

import Data.List (intercalate)
import Text.Parsec ((<|>), (<?>), char, many1, optionMaybe, string, try)

import qualified AST.Type as Type
import Parse.Helpers
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import AST.V0_16


tvar :: IParser Type.Type
tvar =
  addLocation
    (Type.RVar <$> lowVar <?> "a type variable")


tuple :: IParser Type.Type
tuple =
  do  (start, types, end) <- located (parens $ ((\f a b _ -> f a b) <$> commaSep (const . const <$> expr))) -- TODO: use comments
      case types of
        [t] -> return t
        _   -> return (Type.tuple (R.Region start end) types)


record :: IParser Type.Type
record =
  addLocation $
  do  char '{'
      pushNewlineContext
      whitespace -- TODO: use comments
      (ext, fields) <- extended <|> normal
      dumbWhitespace -- TODO: use comments
      char '}'
      sawNewline <- popNewlineContext
      return $ Type.RRecord ext (fields [] []) sawNewline -- TODO: pass comments
  where
    normal =
      do  (\fields -> (Nothing, fields) ) <$> commaSep field

    -- extended record types require at least one field
    extended =
      do  ext <- try (addLocation lowVar <* (whitespace >> string "|")) -- TODO: use comments
          whitespace -- TODO: use comments
          fields <- commaSep1 field
          return ((Just (A.map Type.RVar ext)), fields)

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


constructor0 :: IParser Type.Type
constructor0 =
  addLocation $
  do  name <- capTypeVar
      return (Type.RVar name)


term :: IParser Type.Type
term =
  tuple <|> record <|> tvar <|> constructor0


app :: IParser Type.Type
app =
  do  start <- getMyPosition
      f <- constructor0 <|> try tupleCtor <?> "a type constructor"
      args <- map (\(_,v) -> v) <$> spacePrefix term -- TODO: use comments
      end <- getMyPosition
      case args of
        [] -> return f
        _  -> return (A.A (R.Region start end) (Type.RApp f args))
  where
    tupleCtor =
      addLocation $
      do  ctor <- parens' (many1 (char ','))
          return (Type.RTupleFunction (length ctor + 1))


expr :: IParser Type.Type
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
                    Type.RLambda t2' ts ->
                        return (A.A (R.Region start end) (Type.RLambda t1 (t2':ts)))
                    _ ->
                        return (A.A (R.Region start end) (Type.RLambda t1 [t2]))


constructor :: IParser (String, [Type.Type])
constructor =
  (,) <$> (capTypeVar <?> "another type constructor")
      <*> (map (\(_,v) -> v) <$> spacePrefix term) -- TODO: use comments

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
  addLocation $
  do  char '{'
      pushNewlineContext
      (_, pre) <- whitespace
      body <- extended <|> normal
      post <- dumbWhitespace
      char '}'
      sawNewline <- popNewlineContext
      return $ body pre post sawNewline
  where
    normal =
      do
          fields <- commaSep field
          return $ \pre post sawNewline ->
            case fields pre post of
              [] ->
                EmptyRecordType (pre ++ post)
              fields' ->
                RecordType fields' sawNewline

    -- extended record types require at least one field
    extended =
      do  (ext, postBase) <-
            try $
              do
                ext <- lowVar
                (_, postBase) <- whitespace
                _ <- string "|"
                return (ext, postBase)
          (_, preFields) <- whitespace
          fields <- commaSep1 field
          return $ \pre post sawNewline ->
            RecordExtensionType
              (Commented pre ext postBase)
              (fields preFields post)
              sawNewline

    field =
      do  pushNewlineContext
          lbl <- rLabel
          (_, postLbl) <- whitespace
          _ <- hasType
          (_, preExpr) <- whitespace
          val <- expr
          sawNewline <- popNewlineContext
          return $ \preLbl postExpr ->
            ( Commented preLbl lbl postLbl
            , Commented preExpr val postExpr
            , sawNewline
            )


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
  addLocation $
  do  f <- constructor0 <|> try tupleCtor <?> "a type constructor"
      args <- spacePrefix term
      return $ TypeConstruction f args


expr :: IParser Type
expr =
  do  start <- getMyPosition
      t1 <- app <|> term
      arrow <- optionMaybe $ try (whitespace <* rightArrow)
      case arrow of
        Nothing ->
            return t1
        Just (_, preArrow) ->
            do  (_, postArrow) <- whitespace
                t2 <- expr
                end <- getMyPosition
                return $ A.A (R.Region start end) $
                  case A.drop t2 of
                      FunctionType (t2',postT2) ts tlast ->
                          FunctionType (t1, preArrow) ((Commented postArrow t2' postT2):ts) tlast
                      _ ->
                          FunctionType (t1, preArrow) [] (postArrow, t2)


constructor :: IParser (String, [Type])
constructor =
  (,) <$> (capTypeVar <?> "another type constructor")
      <*> (map (\(_,v) -> v) <$> spacePrefix term) -- TODO: use comments

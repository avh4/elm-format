module Parse.Pattern (term, expr) where

import Data.Char (isUpper)
import qualified Data.List as List
import Text.Parsec ((<|>), (<?>), char, choice, optionMaybe, try)

import AST.V0_16
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import Parse.Helpers
import qualified Parse.Literal as Literal
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


basic :: IParser P.Pattern
basic =
  addLocation $
    choice
      [ char '_' >> return P.Anything
      , stringToPattern <$> var
      , P.Literal <$> Literal.literal
      ]
  where
    stringToPattern str =
        case str of
          "True" ->
              P.Literal (Boolean True)

          "False" ->
              P.Literal (Boolean False)

          c:_ | isUpper c ->
              P.Data (Var.VarRef str) []

          _ ->
              P.Var (Var.VarRef str)


asPattern :: IParser P.Pattern -> IParser P.Pattern
asPattern patternParser =
  do  pattern <- patternParser

      let (A.A (R.Region start _) _) = pattern

      maybeAlias <- optionMaybe asAlias

      case maybeAlias of
        Just alias ->
            do  end <- getMyPosition
                return (A.at start end (P.Alias alias pattern))

        Nothing ->
            return pattern
  where
    asAlias =
      do  try (whitespace >> reserved "as") -- TODO: use comments
          whitespace -- TODO: use comments
          lowVar


record :: IParser P.Pattern
record =
  addLocation $
  do
      v <- brackets ((\f a b _ -> f a b) <$> commaSep1 (const <$> const <$> lowVar)) -- TODO: use comments
      return $ P.Record v


tuple :: IParser P.Pattern
tuple =
  do  (start, patterns, end) <-
          located (parens $ ((\f a b _ -> f a b) <$> commaSep (const <$> const <$> expr))) -- TODO: use comments

      case patterns of
        [pattern] ->
            return pattern

        _ ->
            return (A.at start end (P.Tuple patterns))


list :: IParser P.Pattern
list =
  braces $
    do  (start, patterns, end) <- located (commaSep (const . const <$> expr)) -- TODO: use comments
        return $ \_ _ _ -> A.at start end $ P.List (patterns [] []) -- TODO: use comments


term :: IParser P.Pattern
term =
  choice [ record, tuple, list, basic ]
    <?> "a pattern"


patternConstructor :: IParser P.Pattern
patternConstructor =
  addLocation $
    do  v <- List.intercalate "." <$> dotSep1 capVar
        case v of
          "True"  -> return $ P.Literal (Boolean True)
          "False" -> return $ P.Literal (Boolean False)
          _       -> P.Data (Var.VarRef v) <$> map (\(_,v) -> v) <$> spacePrefix term -- TODO: use comments


expr :: IParser P.Pattern
expr =
    asPattern subPattern <?> "a pattern"
  where
    subPattern =
      do  patterns <- consSep1 (const . const <$> (patternConstructor <|> term)) -- TODO: use comments
          end <- getMyPosition
          return (P.consMany end (patterns [] [])) -- TODO: pass comments

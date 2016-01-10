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
              P.Data str []

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
      v <- brackets ((\f a b _ -> f a b) <$> commaSep1 ((\x pre post -> Commented pre x post) <$> lowVar))
      return $ P.Record v


tuple :: IParser P.Pattern
tuple =
  do  (start, patterns, end) <- located $ parens'' expr

      return $
        case patterns of
          Left comments ->
            A.at start end $ P.UnitPattern comments

          Right [] ->
            A.at start end $ P.UnitPattern []

          Right [Commented _ pattern _] -> -- TODO: use comments
            pattern

          Right patterns ->
            A.at start end $ P.Tuple patterns


list :: IParser P.Pattern
list =
  addLocation $
  do
    result <- braces'' expr
    return $
      case result of
        Left comments ->
          P.EmptyListPattern comments
        Right patterns ->
          P.List patterns


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
          _       -> P.Data v <$> map (\(_,v) -> v) <$> spacePrefix term -- TODO: use comments


expr :: IParser P.Pattern
expr =
    asPattern subPattern <?> "a pattern"
  where
    subPattern =
      do  (start, patterns, end) <- located $ consSep1 (const . const <$> (patternConstructor <|> term)) -- TODO: use comments
          return $
            case patterns [] [] of -- TODO: pass comments
              [pattern] ->
                pattern
              (first:second:rest) ->
                A.A (R.Region start end) $ P.ConsPattern first (init $ second:rest) (last $ second:rest)

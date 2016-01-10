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
  do  (start, pattern, _) <- located patternParser

      maybeAlias <- optionMaybe asAlias

      case maybeAlias of
        Just (postPattern, alias) ->
            do  end <- getMyPosition
                return $ A.at start end $ P.Alias (pattern, postPattern) alias

        Nothing ->
            return pattern
  where
    asAlias =
      do  (_, preAs) <- try (whitespace <* reserved "as")
          (_, postAs) <- whitespace
          var <- lowVar
          return (preAs, (postAs, var))


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

          Right [Commented [] pattern []] ->
            pattern

          Right [pattern] ->
            A.at start end $ P.PatternParens pattern

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
          _       -> P.Data v <$> spacePrefix term


expr :: IParser P.Pattern
expr =
    asPattern subPattern <?> "a pattern"
  where
    subPattern =
      do
        result <- separated cons (patternConstructor <|> term)
        return $
          case result of
            Left pattern ->
              pattern
            Right (region, first, rest, final) ->
              A.A region $ P.ConsPattern first rest final

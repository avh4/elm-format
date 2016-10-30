module Parse.Pattern (term, expr) where

import Text.Parsec ((<|>), (<?>), char, choice, optionMaybe, try)

import AST.V0_16
import qualified AST.Pattern as P
import Parse.Helpers
import qualified Parse.Literal as Literal
import qualified Reporting.Annotation as A


basic :: IParser P.Pattern
basic =
  addLocation $
    choice
      [ char '_' >> return P.Anything
      , P.VarPattern <$> lowVar
      , chunksToPattern <$> dotSep1 capVar
      , P.Literal <$> Literal.literal
      ]
  where
    chunksToPattern chunks =
        case chunks of
          [UppercaseIdentifier "True"] ->
              P.Literal (Boolean True)

          [UppercaseIdentifier "False"] ->
              P.Literal (Boolean False)

          name ->
              P.Data name []


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
      do  preAs <- try (whitespace <* reserved "as")
          postAs <- whitespace
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
    do  v <- dotSep1 capVar
        case v of
          [UppercaseIdentifier "True"]  -> return $ P.Literal (Boolean True)
          [UppercaseIdentifier "False"] -> return $ P.Literal (Boolean False)
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
            Right (region, first, rest, final, _) ->
              A.A region $ P.ConsPattern first rest final

module Parse.Pattern (term, expr) where

import Text.Parsec ((<|>), (<?>), char, choice, optionMaybe, try)

import AST.V0_16
import AST.Pattern (Pattern)
import qualified AST.Pattern as P
import ElmVersion
import Parse.Helpers
import qualified Parse.Literal as Literal
import qualified Reporting.Annotation as A
import Parse.IParser
import Parse.Whitespace


basic :: ElmVersion -> IParser (Pattern [UppercaseIdentifier])
basic elmVersion =
  addLocation $
    choice
      [ char '_' >> return P.Anything
      , P.VarPattern <$> lowVar elmVersion
      , chunksToPattern <$> dotSep1 (capVar elmVersion)
      , P.Literal <$> Literal.literal
      ]
  where
    chunksToPattern chunks =
        case reverse chunks of
          [UppercaseIdentifier "True"] ->
              P.Literal (Boolean True)

          [UppercaseIdentifier "False"] ->
              P.Literal (Boolean False)

          (last:rest) ->
              P.Data (reverse rest, last) []


asPattern :: ElmVersion -> IParser (Pattern [UppercaseIdentifier]) -> IParser (Pattern [UppercaseIdentifier])
asPattern elmVersion patternParser =
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
      do  preAs <- try (whitespace <* reserved elmVersion "as")
          postAs <- whitespace
          var <- lowVar elmVersion
          return (preAs, (postAs, var))


record :: ElmVersion -> IParser (Pattern [UppercaseIdentifier])
record elmVersion =
  addLocation $
  do
      result <- surround'' '{' '}' (lowVar elmVersion)
      return $
          case result of
              Left comments ->
                  P.EmptyRecordPattern comments
              Right fields ->
                  P.Record fields


tuple :: ElmVersion -> IParser (Pattern [UppercaseIdentifier])
tuple elmVersion =
  do  (start, patterns, end) <- located $ parens'' (expr elmVersion)

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


list :: ElmVersion -> IParser (Pattern [UppercaseIdentifier])
list elmVersion =
  addLocation $
  do
    result <- braces'' (expr elmVersion)
    return $
      case result of
        Left comments ->
          P.EmptyListPattern comments
        Right patterns ->
          P.List patterns


term :: ElmVersion -> IParser (Pattern [UppercaseIdentifier])
term elmVersion =
  choice [ record elmVersion, tuple elmVersion, list elmVersion, basic elmVersion ]
    <?> "a pattern"


patternConstructor :: ElmVersion -> IParser (Pattern [UppercaseIdentifier])
patternConstructor elmVersion =
  addLocation $
    do  v <- dotSep1 (capVar elmVersion)
        case reverse v of
          [UppercaseIdentifier "True"]  -> return $ P.Literal (Boolean True)
          [UppercaseIdentifier "False"] -> return $ P.Literal (Boolean False)
          (last:rest) -> P.Data (reverse rest, last) <$> spacePrefix (term elmVersion)


expr :: ElmVersion -> IParser (Pattern [UppercaseIdentifier])
expr elmVersion =
    asPattern elmVersion subPattern <?> "a pattern"
  where
    subPattern =
      do
        result <- separated cons (patternConstructor elmVersion <|> term elmVersion)
        return $
          case result of
            Left pattern ->
              pattern
            Right (region, first, rest, _) ->
              A.A region $ P.ConsPattern first rest

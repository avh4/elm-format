{-# LANGUAGE DataKinds #-}
module Parse.Pattern (term, expr) where

import Text.Parsec ((<|>), (<?>), char, choice, optionMaybe, try)

import AST.V0_16
import AST.Structure
import qualified Data.Indexed as I
import ElmVersion
import Parse.Helpers
import qualified Parse.Literal as Literal
import Reporting.Annotation (Located)
import qualified Reporting.Annotation as A
import Parse.IParser
import Parse.Whitespace


basic :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'PatternNK)
basic elmVersion =
  fmap I.Fix $ addLocation $
    choice
      [ char '_' >> return Anything
      , VarPattern <$> lowVar elmVersion
      , chunksToPattern <$> dotSep1 (capVar elmVersion)
      , LiteralPattern <$> Literal.literal
      ]
  where
    chunksToPattern chunks =
        case reverse chunks of
          [UppercaseIdentifier "True"] ->
              LiteralPattern (Boolean True)

          [UppercaseIdentifier "False"] ->
              LiteralPattern (Boolean False)

          (last:rest) ->
              DataPattern (reverse rest, last) []

          [] -> error "dotSep1 returned empty list"


asPattern :: ElmVersion -> IParser (FixAST Located typeRef ctorRef varRef 'PatternNK) -> IParser (FixAST Located typeRef ctorRef varRef 'PatternNK)
asPattern elmVersion patternParser =
  do  (start, pattern, _) <- located patternParser

      maybeAlias <- optionMaybe asAlias

      case maybeAlias of
        Just (postPattern, alias) ->
            do  end <- getMyPosition
                return $ I.Fix $ A.at start end $ Alias (C postPattern pattern) alias

        Nothing ->
            return pattern
  where
    asAlias =
      do  preAs <- try (whitespace <* reserved elmVersion "as")
          postAs <- whitespace
          var <- lowVar elmVersion
          return (preAs, C postAs var)


record :: ElmVersion -> IParser (FixAST Located typeRef ctorRef varRef 'PatternNK)
record elmVersion =
  fmap I.Fix $ addLocation $
  do
      result <- surround'' '{' '}' (lowVar elmVersion)
      return $
          case result of
              Left comments ->
                  EmptyRecordPattern comments
              Right fields ->
                  RecordPattern fields


tuple :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'PatternNK)
tuple elmVersion =
  do  (start, patterns, end) <- located $ parens'' (expr elmVersion)

      return $
        case patterns of
          Left comments ->
            I.Fix $ A.at start end $ UnitPattern comments

          Right [] ->
            I.Fix $ A.at start end $ UnitPattern []

          Right [C ([], []) pattern] ->
            pattern

          Right [pattern] ->
            I.Fix $ A.at start end $ PatternParens pattern

          Right patterns ->
            I.Fix $ A.at start end $ TuplePattern patterns


list :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'PatternNK)
list elmVersion =
  fmap I.Fix $ addLocation $
  do
    result <- braces'' (expr elmVersion)
    return $
      case result of
        Left comments ->
          EmptyListPattern comments
        Right patterns ->
          ListPattern patterns


term :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'PatternNK)
term elmVersion =
  choice [ record elmVersion, tuple elmVersion, list elmVersion, basic elmVersion ]
    <?> "a pattern"


patternConstructor :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'PatternNK)
patternConstructor elmVersion =
  fmap I.Fix $ addLocation $
    do  v <- dotSep1 (capVar elmVersion)
        case reverse v of
          [UppercaseIdentifier "True"]  -> return $ LiteralPattern (Boolean True)
          [UppercaseIdentifier "False"] -> return $ LiteralPattern (Boolean False)
          (last:rest) -> DataPattern (reverse rest, last) <$> spacePrefix (term elmVersion)
          [] -> error "dotSep1 returned empty list"


expr :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'PatternNK)
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
              I.Fix $ A.A region $ ConsPattern first rest

{-# LANGUAGE DataKinds #-}
module Parse.Parse (parse, parseModule, parseDeclarations, parseExpressions) where

import Parse.ParsecAdapter (eof)
import qualified Parse.ParsecAdapter as Parsec

import AST.V0_16
import AST.Module (Module)
import AST.Structure
import ElmVersion hiding (parse)
import Parse.Comments (withEol)
import qualified Parse.Declaration
import qualified Parse.Expression
import Parse.Helpers
import qualified Parse.Module
import Reporting.Annotation (Located)
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Result as Result
import Parse.IParser
import qualified Data.Indexed as I


parseModule :: ElmVersion -> String -> Result.Result () Error.Error (Module [UppercaseIdentifier] (I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'TopLevelNK))
parseModule elmVersion src =
    parse src (Parse.Module.elmModule elmVersion)


parseDeclarations :: ElmVersion -> String -> Result.Result () Error.Error [TopLevelStructure (I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'TopLevelDeclarationNK)]
parseDeclarations elmVersion src =
    parse src (Parse.Module.topLevel (Parse.Declaration.declaration elmVersion) <* eof)


parseExpressions :: ElmVersion -> String -> Result.Result () Error.Error [TopLevelStructure (C0Eol (I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'ExpressionNK))]
parseExpressions elmVersion src =
    parse src (Parse.Module.topLevel (withEol $ Parse.Expression.expr elmVersion) <* eof)


-- RUN PARSERS

parse :: String -> IParser a -> Result.Result wrn Error.Error a
parse source parser =
  case iParse parser source of
    Right result ->
        return result

    Left err ->
        let
          pos = (Parsec.errorPos err)
        in
        Result.throw (A.Region pos pos) (Error.Parse err)

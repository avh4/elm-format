module Parse.Parse (parse, parseModule, parseDeclarations, parseExpressions) where

import Parse.ParsecAdapter (eof)
import qualified Parse.ParsecAdapter as Parsec

import AST.V0_16
import ElmVersion hiding (parse)
import Parse.Comments (withEol)
import qualified Parse.Declaration
import qualified Parse.Expression
import Parse.Helpers
import qualified Parse.Module
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Result as Result
import Parse.IParser
import Data.ByteString (ByteString)


parseModule :: ElmVersion -> ByteString -> Result.Result () Error.Error (ParsedAST 'ModuleNK)
parseModule elmVersion src =
    parse src (Parse.Module.elmModule elmVersion)


parseDeclarations :: ElmVersion -> ByteString -> Result.Result () Error.Error [TopLevelStructure (ParsedAST 'TopLevelDeclarationNK)]
parseDeclarations elmVersion src =
    parse src (Parse.Module.topLevel (Parse.Declaration.declaration elmVersion) <* eof)


parseExpressions :: ElmVersion -> ByteString -> Result.Result () Error.Error [TopLevelStructure (C0Eol (ParsedAST 'ExpressionNK))]
parseExpressions elmVersion src =
    parse src (Parse.Module.topLevel (withEol $ Parse.Expression.expr elmVersion) <* eof)


-- RUN PARSERS

parse :: ByteString -> IParser a -> Result.Result wrn Error.Error a
parse source parser =
  case iParse parser source of
    Right result ->
        return result

    Left err ->
        let
          pos = Parsec.errorPos err
        in
        Result.throw (A.Region pos pos) (Error.Parse err)

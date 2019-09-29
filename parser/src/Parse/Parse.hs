{-# OPTIONS_GHC -Wall #-}
module Parse.Parse (parse, parseModule, parseDeclarations, parseExpressions) where

import qualified Text.Parsec.Error as Parsec

import AST.V0_16
import AST.Declaration (TopLevelStructure, Declaration)
import AST.Expression (Expr)
import qualified AST.Module
import ElmVersion hiding (parse)
import Parse.Comments (withEol)
import qualified Parse.Declaration
import qualified Parse.Expression
import Parse.Helpers
import qualified Parse.Module
import qualified Reporting.Region as R
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Result as Result
import Parse.IParser
import Text.Parsec (eof)


parseModule :: ElmVersion -> String -> Result.Result () Error.Error AST.Module.Module
parseModule elmVersion src =
    parse src (Parse.Module.elmModule elmVersion)


parseDeclarations :: ElmVersion -> String -> Result.Result () Error.Error [TopLevelStructure (Declaration [UppercaseIdentifier] Expr)]
parseDeclarations elmVersion src =
    parse src (Parse.Module.topLevel (Parse.Declaration.declaration elmVersion) <* eof)


parseExpressions :: ElmVersion -> String -> Result.Result () Error.Error [TopLevelStructure (WithEol Expr)]
parseExpressions elmVersion src =
    parse src (Parse.Module.topLevel (withEol $ Parse.Expression.expr elmVersion) <* eof)


-- RUN PARSERS

parse :: String -> IParser a -> Result.Result wrn Error.Error a
parse source parser =
  case iParse parser source of
    Right result ->
        return result

    Left err ->
        let pos = R.fromSourcePos (Parsec.errorPos err)
            msgs = Parsec.errorMessages err
        in
            Result.throw (R.Region pos pos) (Error.Parse msgs)

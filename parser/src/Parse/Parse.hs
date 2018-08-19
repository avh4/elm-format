{-# OPTIONS_GHC -Wall #-}
module Parse.Parse (parse, parseModule, parseDeclarations, parseExpressions) where

import qualified Text.Parsec.Error as Parsec

import AST.V0_16 (WithEol)
import AST.Declaration (TopLevelStructure, Declaration)
import qualified AST.Expression
import qualified AST.Module
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


parseModule :: String -> Result.Result () Error.Error AST.Module.Module
parseModule src =
  parse src Parse.Module.elmModule


parseDeclarations :: String -> Result.Result () Error.Error [TopLevelStructure Declaration]
parseDeclarations src =
    parse src (Parse.Module.topLevel Parse.Declaration.declaration <* eof)


parseExpressions :: String -> Result.Result () Error.Error [TopLevelStructure (WithEol AST.Expression.Expr)]
parseExpressions src =
    parse src (Parse.Module.topLevel (withEol Parse.Expression.expr) <* eof)


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

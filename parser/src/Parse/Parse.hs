{-# OPTIONS_GHC -Wall #-}
module Parse.Parse (parse, parseSource) where

import qualified Text.Parsec.Error as Parsec

import qualified AST.Module
import Parse.Helpers
import qualified Parse.Module as Module
import qualified Reporting.Region as R
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Result as Result
import Parse.IParser


parseSource :: String -> Result.Result () Error.Error AST.Module.Module
parseSource src =
  parse src Module.elmModule


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

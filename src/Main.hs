{-# OPTIONS_GHC -Wall #-}
module Main where

import Elm.Utils ((|>))
import System.Exit (exitFailure)

import qualified AST.Module as M
import qualified Elm.Package
import qualified Flags
import qualified Format
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Reporting.Annotation as RA
import qualified Reporting.Error as Error
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Report as Report
import qualified Reporting.Result as Result
import qualified Parse.Parse as Parse


formatResult
    :: Flags.Config
    -> Result.Result () Syntax.Error M.SourceModule
    -> IO ()
formatResult config result =
    case result of
        Result.Result _ (Result.Ok mod) ->
            LazyText.writeFile (Flags._output config)
                $ LazyText.pack $ Format.formatModule (flip (++)) mod ""
        Result.Result _ (Result.Err errs) ->
            do
                LazyText.writeFile (Flags._output config)
                    $ LazyText.pack ""

                putStrLn "ERRORS"
                sequence $ map printError errs
                exitFailure


printError :: RA.Located Syntax.Error -> IO ()
printError (RA.A range err) =
    Report.printError "<location>" range (Syntax.toReport Map.empty err) ""


main :: IO ()
main =
    do  config <- Flags.parse

        input <- LazyText.readFile (Flags._file config)

        formatResult config $ Parse.parseSource $ LazyText.unpack input

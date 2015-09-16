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
import qualified Parse.Parse as Parse
import qualified Reporting.Annotation as RA
import qualified Reporting.Error as Error
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Report as Report
import qualified Reporting.Result as Result
import qualified Text.PrettyPrint.Boxes as Box


formatResult
    :: Flags.Config
    -> Result.Result () Syntax.Error M.SourceModule
    -> IO ()
formatResult config result =
    case result of
        Result.Result _ (Result.Ok mod) ->
            Format.formatModule mod
                |> Box.render
                |> LazyText.pack
                |> trimSpaces
                |> LazyText.writeFile (Flags._output config)
        Result.Result _ (Result.Err errs) ->
            do
                LazyText.writeFile (Flags._output config)
                    $ LazyText.pack ""

                putStrLn "ERRORS"
                sequence $ map printError errs
                exitFailure
    where
        trimSpaces = LazyText.unlines . (map LazyText.stripEnd) . LazyText.lines


printError :: RA.Located Syntax.Error -> IO ()
printError (RA.A range err) =
    Report.printError "<location>" range (Syntax.toReport Map.empty err) ""


main :: IO ()
main =
    do  config <- Flags.parse

        input <- LazyText.readFile (Flags._file config)

        formatResult config $ Parse.parseSource $ LazyText.unpack input

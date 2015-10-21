{-# OPTIONS_GHC -Wall #-}
module Main where

import Elm.Utils ((|>))
import System.Exit (exitFailure)
import Control.Monad (when)

import qualified AST.Module
import qualified Box
import qualified Flags
import qualified Format
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Parse.Parse as Parse
import qualified Reporting.Annotation as RA
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Report as Report
import qualified Reporting.Result as Result


formatResult
    :: Flags.Config
    -> Result.Result () Syntax.Error AST.Module.Module
    -> IO ()
formatResult config result =
    case result of
        Result.Result _ (Result.Ok modu) ->
            Format.formatModule modu
                |> Box.render
                |> LazyText.pack
                |> trimSpaces
                |> LazyText.writeFile outputFile
        Result.Result _ (Result.Err errs) ->
            do
                when (not isOutputEmpty)
                  (LazyText.writeFile (Flags._output config)
                    $ LazyText.pack "")

                putStrLn "ERRORS"
                _ <- sequence $ map printError errs
                exitFailure
    where
        trimSpaces = LazyText.unlines . (map LazyText.stripEnd) . LazyText.lines
        isOutputEmpty = null (Flags._output config)
        outputFile = if isOutputEmpty
                         then Flags._file config
                         else Flags._output config


printError :: RA.Located Syntax.Error -> IO ()
printError (RA.A range err) =
    Report.printError "<location>" range (Syntax.toReport err) ""


main :: IO ()
main =
    do  config <- Flags.parse

        input <- LazyText.readFile (Flags._file config)

        formatResult config $ Parse.parseSource $ LazyText.unpack input

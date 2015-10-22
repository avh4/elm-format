{-# OPTIONS_GHC -Wall #-}
module Main where

import Elm.Utils ((|>))
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when)
import Data.Maybe (isJust, fromMaybe)
import Data.Char (toLower)

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



createEmptyFile :: Maybe FilePath -> IO ()
createEmptyFile (Just outputFile) = LazyText.writeFile outputFile $ LazyText.pack ""
createEmptyFile _                 = return ()

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
                createEmptyFile givenOutput

                putStrLn "ERRORS"
                _ <- sequence $ map printError errs
                exitFailure
    where
        trimSpaces = LazyText.unlines . (map LazyText.stripEnd) . LazyText.lines
        givenInput = Flags._input config
        givenOutput = Flags._output config
        outputFile = fromMaybe givenInput givenOutput


printError :: RA.Located Syntax.Error -> IO ()
printError (RA.A range err) =
    Report.printError "<location>" range (Syntax.toReport err) ""

promptRewriting :: FilePath -> String
promptRewriting filePath =
    unlines
        [ "This will overwrite the following files to use Elm’s preferred style:"
        , ""
        , "  " ++ filePath
        , ""
        , "This cannot be undone! Make sure to back up these files before proceeding."
        , ""
        , "Are you sure you want to overwrite these files with formatted versions? (y/N)"
        ]

main :: IO ()
main =
    do  config <- Flags.parse

        case (Flags._output config) of
            Nothing -> do -- we are overwriting the input file
                when (not $ Flags._yes config) $ do
                    putStrLn $ promptRewriting (Flags._input config)
                    answer <- getLine
                    when ((map toLower answer) == "n") $
                        exitSuccess
            Just _ -> return ()

        input <- LazyText.readFile (Flags._input config)

        formatResult config $ Parse.parseSource $ LazyText.unpack input

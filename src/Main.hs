{-# OPTIONS_GHC -Wall #-}
module Main where

import Elm.Utils ((|>))
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when)
import Data.Maybe (isJust, fromMaybe)

import qualified AST.Module
import qualified Box
import qualified CommandLine.Helpers as Cmd
import qualified Flags
import qualified Format
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Parse.Parse as Parse
import qualified Reporting.Annotation as RA
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Report as Report
import qualified Reporting.Result as Result



maybeCreateEmptyFile :: Maybe FilePath -> IO ()
maybeCreateEmptyFile (Just outputFile) = LazyText.writeFile outputFile $ LazyText.pack ""
maybeCreateEmptyFile _                 = return ()


showErrors :: [RA.Located Syntax.Error] -> IO ()
showErrors errs = do
    putStrLn "ERRORS"
    mapM_ printError errs


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
                maybeCreateEmptyFile givenOutput
                showErrors errs
                exitFailure
    where
        trimSpaces = LazyText.unlines . (map LazyText.stripEnd) . LazyText.lines
        givenInput = Flags._input config
        givenOutput = Flags._output config
        outputFile = fromMaybe givenInput givenOutput


printError :: RA.Located Syntax.Error -> IO ()
printError (RA.A range err) =
    Report.printError "<location>" range (Syntax.toReport err) ""

getApproval :: Bool -> FilePath -> IO Bool
getApproval autoYes filePath =
    case autoYes of
        True ->
            return True
        False -> do
            putStrLn "This will overwrite the following files to use Elm’s preferred style:\n"
            putStrLn $ "  " ++ filePath ++ "\n"
            putStrLn "This cannot be undone! Make sure to back up these files before proceeding.\n"
            putStr "Are you sure you want to overwrite these files with formatted versions? (y/n) "
            Cmd.yesOrNo

main :: IO ()
main =
    do  config <- Flags.parse

        case outputFile of
            Nothing -> do -- we are overwriting the input file
                canOverwrite <- getApproval autoYes inputFile
                case canOverwrite of
                    True -> return ()
                    False -> exitSuccess
            Just _ -> return ()

        input <- LazyText.readFile inputFile

        formatResult config $ Parse.parseSource $ LazyText.unpack input
    where
        inputFile = (Flags._input config)
        outputFile = (Flags._output config)
        autoYes = (Flags._yes config)

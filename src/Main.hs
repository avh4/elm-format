{-# OPTIONS_GHC -Wall #-}
module Main where

import Elm.Utils ((|>))
import System.Exit (exitFailure, exitSuccess)
import System.FilePath.Find (find, (==?), extension, always)
import Control.Monad (when)
import Data.Maybe (isJust)

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
import qualified System.Directory as Dir


showErrors :: [RA.Located Syntax.Error] -> IO ()
showErrors errs = do
    putStrLn "ERRORS"
    mapM_ printError errs


writeEmptyFile :: FilePath -> IO ()
writeEmptyFile filePath =
    LazyText.writeFile filePath $ LazyText.pack ""


formatResult
    :: Bool
    -> FilePath
    -> Result.Result () Syntax.Error AST.Module.Module
    -> IO ()
formatResult canWriteEmptyFileOnError outputFile result =
    case result of
        Result.Result _ (Result.Ok modu) ->
            Format.formatModule modu
                |> Box.render
                |> LazyText.pack
                |> trimSpaces
                |> LazyText.writeFile outputFile
        Result.Result _ (Result.Err errs) ->
            do
                when canWriteEmptyFileOnError $
                    writeEmptyFile outputFile

                showErrors errs
                exitFailure
    where
        trimSpaces = LazyText.unlines . (map LazyText.stripEnd) . LazyText.lines


printError :: RA.Located Syntax.Error -> IO ()
printError (RA.A range err) =
    Report.printError "<location>" range (Syntax.toReport err) ""

getApproval :: Bool -> [FilePath] -> IO Bool
getApproval autoYes filePaths =
    case autoYes of
        True ->
            return True
        False -> do
            putStrLn "This will overwrite the following files to use Elm’s preferred style:\n"
            mapM_ (\filePath -> putStrLn $ "  " ++ filePath) filePaths
            putStrLn "\nThis cannot be undone! Make sure to back up these files before proceeding.\n"
            putStr "Are you sure you want to overwrite these files with formatted versions? (y/n) "
            Cmd.yesOrNo

exitFileNotFound :: FilePath -> IO ()
exitFileNotFound filePath = do
    putStrLn "Could not find any .elm file on the specified path:\n"
    putStrLn $ "  " ++ filePath ++ "\n"
    putStrLn "Please check the given path."
    exitFailure

exitOnInputDirAndOutput :: IO ()
exitOnInputDirAndOutput = do
    putStrLn "Can't write to the OUTPUT path, because INPUT path is a directory.\n"
    putStrLn "Please remove the OUTPUT argument. The .elm files in INPUT will be formatted in place."
    exitFailure

processFile :: FilePath -> FilePath -> IO ()
processFile inputFile outputFile =
    do
        putStrLn $ "Processing file " ++ inputFile
        input <- LazyText.readFile inputFile
        LazyText.unpack input
            |> Parse.parseSource
            |> formatResult canWriteEmptyFileOnError outputFile
    where
        canWriteEmptyFileOnError = outputFile /= inputFile

decideOutputFile :: Bool -> FilePath -> Maybe FilePath -> IO FilePath
decideOutputFile autoYes inputFile outputFile =
    case outputFile of
        Nothing -> do -- we are overwriting the input file
            canOverwrite <- getApproval autoYes [inputFile]
            case canOverwrite of
                True -> return inputFile
                False -> exitSuccess
        Just outputFile' -> return outputFile'

findAllElmFiles :: FilePath -> IO [FilePath]
findAllElmFiles inputFile =
    find always (extension ==? ".elm") inputFile


main :: IO ()
main =
    do
        config <- Flags.parse
        let inputFile = (Flags._input config)
        let outputFile = (Flags._output config)
        let autoYes = (Flags._yes config)

        fileExists <- Dir.doesFileExist inputFile
        dirExists <- Dir.doesDirectoryExist inputFile

        when (not (fileExists || dirExists)) $
            exitFileNotFound inputFile

        if fileExists
            then do
                realOutputFile <- decideOutputFile autoYes inputFile outputFile
                processFile inputFile realOutputFile
            else do -- dirExists
                when (isJust outputFile)
                    exitOnInputDirAndOutput

                elmFiles <- findAllElmFiles inputFile
                when (null elmFiles) $
                    exitFileNotFound inputFile

                canOverwriteFiles <- getApproval autoYes elmFiles
                when canOverwriteFiles $
                    mapM_ (\file -> processFile file file) elmFiles

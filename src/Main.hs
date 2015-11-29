{-# OPTIONS_GHC -Wall #-}
module Main where

import Elm.Utils ((|>))
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when)
import Data.Maybe (isJust)
import Messages.Types (Message(..))
import Messages.Strings (renderMessage)

import qualified AST.Module
import qualified CommandLine.Helpers as Cmd
import qualified Flags
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmFormat.Filesystem as FS
import qualified Reporting.Annotation as RA
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Report as Report
import qualified Reporting.Result as Result
import qualified System.Directory as Dir


r :: Message -> String
r = renderMessage

showErrors :: [RA.Located Syntax.Error] -> IO ()
showErrors errs = do
    putStrLn (r ErrorsHeading)
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
            Render.render modu
                |> LazyText.writeFile outputFile

        Result.Result _ (Result.Err errs) ->
            do
                when canWriteEmptyFileOnError $
                    writeEmptyFile outputFile

                showErrors errs
                exitFailure


printError :: RA.Located Syntax.Error -> IO ()
printError (RA.A range err) =
    Report.printError (r ErrorFileLocation) range (Syntax.toReport err) ""


getApproval :: Bool -> [FilePath] -> IO Bool
getApproval autoYes filePaths =
    case autoYes of
        True ->
            return True
        False -> do
            putStrLn $ (r FollowingFilesWillBeOverwritten) ++ "\n"
            mapM_ (\filePath -> putStrLn $ "  " ++ filePath) filePaths
            putStrLn $ "\n" ++ (r BackupFilesBeforeOverwriting) ++ "\n"
            putStr $ (r ConfirmOverwriting) ++ " "
            Cmd.yesOrNo


exitFilesNotFound :: [FilePath] -> IO ()
exitFilesNotFound (filePath:filePaths) = do
    putStrLn $ (r NoElmFilesOnPath) ++ "\n"
    putStrLn $ "  " ++ filePath ++ "\n"
    putStrLn (r PleaseCheckPath)

    case filePaths of
        [] -> exitFailure
        _ -> exitFilesNotFound filePaths

exitOnInputDirAndOutput :: IO ()
exitOnInputDirAndOutput = do
    putStrLn $ (r CantWriteToOutputBecauseInputIsDirectory) ++ "\n"
    putStrLn (r PleaseRemoveOutputArgument)
    exitFailure


processFile :: FilePath -> FilePath -> IO ()
processFile inputFile outputFile =
    do
        putStrLn $ (r ProcessingFile) ++ " " ++ inputFile
        input <- LazyText.readFile inputFile
        Parse.parse input
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

doPathsExist :: (FilePath -> IO Bool) -> [FilePath] -> IO Bool
doPathsExist exists paths =
    do
        case paths of
            [] -> return False
            file:tails -> do
                fileExists <- exists file

                if fileExists
                    then
                        doPathsExist exists tails
                    else
                        return False

doFilesExist :: [FilePath] -> IO Bool
doFilesExist = doPathsExist Dir.doesFileExist

doDirsExist :: [FilePath] -> IO Bool
doDirsExist = doPathsExist Dir.doesDirectoryExist

main :: IO ()
main =
    do
        config <- Flags.parse
        let inputFiles = (Flags._input config)
        let outputFile = (Flags._output config)
        let autoYes = (Flags._yes config)

        fileExists <- doFilesExist inputFiles
        dirExists <- doDirsExist inputFiles

        when (not (fileExists || dirExists)) $
            exitFilesNotFound inputFiles

        if fileExists
            then do
                case inputFiles of
                    inputFile:[] ->
                        do
                            realOutputFile <- decideOutputFile autoYes inputFile outputFile
                            processFile inputFile realOutputFile
                    otherwise ->
                        do
                            canOverwriteFiles <- getApproval autoYes inputFiles
                            mapM_ (\file -> processFile file file) inputFiles


            else do -- dirExists
                when (isJust outputFile)
                    exitOnInputDirAndOutput

                elmFiles <- concat <$> mapM FS.findAllElmFiles inputFiles

                when (null elmFiles) $
                    exitFilesNotFound inputFiles

                canOverwriteFiles <- getApproval autoYes elmFiles
                when canOverwriteFiles $
                    mapM_ (\file -> processFile file file) elmFiles

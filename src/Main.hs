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
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
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


writeFile' :: FilePath -> LazyText.Text -> IO ()
writeFile' filename contents =
    ByteString.writeFile filename $ LazyText.encodeUtf8 contents


writeEmptyFile :: FilePath -> IO ()
writeEmptyFile filePath =
    writeFile' filePath $ LazyText.pack ""


formatResult
    :: Bool
    -> FilePath
    -> Result.Result () Syntax.Error AST.Module.Module
    -> IO ()
formatResult canWriteEmptyFileOnError outputFile result =
    case result of
        Result.Result _ (Result.Ok modu) ->
            Render.render modu
                |> writeFile' outputFile

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
            putStrLn $ (r $ FilesWillBeOverwritten filePaths)
            Cmd.yesOrNo


exitFilesNotFound :: [FilePath] -> IO ()
exitFilesNotFound filePaths = do
    putStrLn $ (r $ NoElmFilesFound filePaths)
    exitFailure


exitOnInputDirAndOutput :: IO ()
exitOnInputDirAndOutput = do
    putStrLn $ r CantWriteToOutputBecauseInputIsDirectory
    exitFailure


processFile :: FilePath -> FilePath -> IO ()
processFile inputFile outputFile =
    do
        putStrLn $ (r ProcessingFile) ++ " " ++ inputFile
        input <- fmap LazyText.decodeUtf8 $ ByteString.readFile inputFile
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

isEitherFileOrDirectory :: FilePath -> IO Bool
isEitherFileOrDirectory path = do
    fileExists <- Dir.doesFileExist path
    dirExists <- Dir.doesDirectoryExist path
    return $ fileExists || dirExists

main :: IO ()
main =
    do
        config <- Flags.parse
        let inputFiles = (Flags._input config)
        let outputFile = (Flags._output config)
        let autoYes = (Flags._yes config)

        filesExist <-
            all (id) <$> mapM isEitherFileOrDirectory inputFiles

        when (not filesExist) $
            exitFilesNotFound inputFiles

        elmFiles <- concat <$> mapM FS.findAllElmFiles inputFiles

        when (null elmFiles) $
            exitFilesNotFound inputFiles

        case elmFiles of
            inputFile:[] -> do
                realOutputFile <- decideOutputFile autoYes inputFile outputFile
                processFile inputFile realOutputFile
            _ -> do
                when (isJust outputFile)
                    exitOnInputDirAndOutput

                canOverwriteFiles <- getApproval autoYes elmFiles
                when canOverwriteFiles $
                    mapM_ (\file -> processFile file file) elmFiles

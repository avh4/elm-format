{-# OPTIONS_GHC -Wall #-}
module Main where

import Elm.Utils ((|>))
import System.Exit (exitFailure, exitSuccess)
import Messages.Types (Message(..))
import Control.Monad (when)
import Data.Maybe (isJust)
import CommandLine.Helpers


import qualified AST.Module
import qualified Flags
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmFormat.Filesystem as FS
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Result as Result
import qualified System.Directory as Dir


writeFile' :: FilePath -> Text.Text -> IO ()
writeFile' filename contents =
    ByteString.writeFile filename $ Text.encodeUtf8 contents

-- If elm-format was successful, writes the results to the output
-- file. Otherwise, display errors and exit
writeResult
    :: FilePath
    -> Result.Result () Syntax.Error AST.Module.Module
    -> IO ()
writeResult outputFile result =
    case result of
        Result.Result _ (Result.Ok modu) ->
            Render.render modu
                |> writeFile' outputFile

        Result.Result _ (Result.Err errs) ->
            do
                showErrors errs
                exitFailure

processFile :: FilePath -> FilePath -> IO ()
processFile inputFile outputFile =
    do
        putStrLn $ (r $ ProcessingFile inputFile)
        input <- fmap Text.decodeUtf8 $ ByteString.readFile inputFile
        Parse.parse input
            |> writeResult outputFile


isEitherFileOrDirectory :: FilePath -> IO Bool
isEitherFileOrDirectory path = do
    fileExists <- Dir.doesFileExist path
    dirExists <- Dir.doesDirectoryExist path
    return $ fileExists || dirExists

-- read input from stdin
-- if given an output file, then write there
-- otherwise, stdout
handleStdinInput :: Maybe FilePath -> IO ()
handleStdinInput outputFile = do
    input <- Lazy.getContents

    let parsedText = Parse.parse $ Text.decodeUtf8 $ Lazy.toStrict input

    case parsedText of
        Result.Result _ (Result.Ok modu) ->
            do
                let rendered = Render.render modu |> Text.encodeUtf8

                case outputFile of
                    Nothing ->
                        Char8.putStrLn rendered

                    Just path -> do
                        writeResult path parsedText

        Result.Result _ (Result.Err errs) ->
            do
                showErrors errs
                exitFailure

handleFilesInput :: [FilePath] -> Maybe FilePath -> Bool -> IO ()
handleFilesInput inputFiles outputFile autoYes =
    do
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

main :: IO ()
main =
    do
        config <- Flags.parse
        let inputFiles = (Flags._input config)
        let isStdin = (Flags._stdin config)
        let outputFile = (Flags._output config)
        let autoYes = (Flags._yes config)

        let noInputSource = null inputFiles && not isStdin
        let twoInputSources = (not $ null inputFiles) && isStdin

        -- when we don't have any input, stdin or otherwise
        when (noInputSource) $ do
            Flags.showHelpText
            exitFailure

        when (twoInputSources) $ do
            exitTooManyInputSources

        when (isStdin) $ do
            handleStdinInput outputFile
            exitSuccess

        handleFilesInput inputFiles outputFile autoYes

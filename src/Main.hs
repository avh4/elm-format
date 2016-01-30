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


-- If elm-format was successful, writes the results to the output
-- file. Otherwise, display errors and exit
writeResult
    :: Maybe FilePath
    -> Text.Text
    -> Result.Result () Syntax.Error AST.Module.Module
    -> IO ()
writeResult outputFile inputText result =
    case result of
        Result.Result _ (Result.Ok modu) ->
            let rendered =
                    Render.render modu
                        |> Text.encodeUtf8
            in
                case outputFile of
                    Nothing ->
                        Char8.putStr rendered

                    Just path -> do
                        ByteString.writeFile path rendered

        Result.Result _ (Result.Err errs) ->
            do
                showErrors (Text.unpack inputText) errs
                exitFailure


processTextInput :: Maybe FilePath -> Text.Text -> IO ()
processTextInput outputFile inputText  =
    Parse.parse inputText
        |> writeResult outputFile inputText


processFileInput :: FilePath -> Maybe FilePath -> IO ()
processFileInput inputFile outputFile =
    do
        putStrLn $ (r $ ProcessingFile inputFile)
        inputText <- fmap Text.decodeUtf8 $ ByteString.readFile inputFile
        processTextInput outputFile inputText


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

    Lazy.toStrict input
        |> Text.decodeUtf8
        |> processTextInput outputFile


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
                processFileInput inputFile (Just realOutputFile)
            _ -> do
                when (isJust outputFile)
                    exitOnInputDirAndOutput

                canOverwriteFiles <- getApproval autoYes elmFiles
                when canOverwriteFiles $
                    mapM_ (\file -> processFileInput file (Just file)) elmFiles

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

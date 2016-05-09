{-# OPTIONS_GHC -Wall #-}
module Main where

import Elm.Utils ((|>))
import System.Exit (exitFailure, exitSuccess)
import Messages.Types (Message(..))
import Control.Monad (when)
import Data.Maybe (isJust)
import CommandLine.Helpers
import ElmVersion (ElmVersion)


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
    -> String
    -> Text.Text
    -> Bool
    -> ElmVersion
    -> Result.Result () Syntax.Error AST.Module.Module
    -> IO (Maybe Bool)
writeResult outputFile inputFilename inputText validateOnly elmVersion result =
    case result of
        Result.Result _ (Result.Ok modu) ->
            let
                renderedText =
                    Render.render elmVersion modu
                rendered =
                    renderedText
                        |> Text.encodeUtf8
            in
                case outputFile of
                    Nothing ->
                        (Char8.putStr rendered)
                        >> (return Nothing)

                    Just path -> do
                        case validateOnly of
                            True ->
                                if inputText /= renderedText then
                                    (putStrLn $ r $ FileWouldChange inputFilename)
                                    >> (return $ Just False)
                                else
                                    return $ Just True
                            False ->
                                (ByteString.writeFile path rendered)
                                >> (return Nothing)

        Result.Result _ (Result.Err errs) ->
            do
                showErrors inputFilename (Text.unpack inputText) errs
                exitFailure


processTextInput :: ElmVersion -> Maybe FilePath -> String -> Bool -> Text.Text -> IO (Maybe Bool)
processTextInput elmVersion outputFile inputFilename validateOnly inputText =
    Parse.parse inputText
        |> writeResult outputFile inputFilename inputText validateOnly elmVersion


processFileInput :: FilePath -> Maybe FilePath -> Bool -> ElmVersion -> IO (Maybe Bool)
processFileInput inputFile outputFile validateOnly elmVersion =
    do
        putStrLn $ (r $ ProcessingFile inputFile)
        inputText <- fmap Text.decodeUtf8 $ ByteString.readFile inputFile
        processTextInput elmVersion outputFile inputFile validateOnly inputText


isEitherFileOrDirectory :: FilePath -> IO Bool
isEitherFileOrDirectory path = do
    fileExists <- Dir.doesFileExist path
    dirExists <- Dir.doesDirectoryExist path
    return $ fileExists || dirExists

-- read input from stdin
-- if given an output file, then write there
-- otherwise, stdout
handleStdinInput :: Maybe FilePath -> ElmVersion -> IO (Maybe Bool)
handleStdinInput outputFile elmVersion = do
    input <- Lazy.getContents

    Lazy.toStrict input
        |> Text.decodeUtf8
        |> processTextInput elmVersion outputFile "<STDIN>" False


handleFilesInput :: [FilePath] -> Maybe FilePath -> Bool -> Bool -> ElmVersion -> IO (Maybe Bool)
handleFilesInput inputFiles outputFile autoYes validateOnly elmVersion =
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
                processFileInput inputFile (Just realOutputFile) validateOnly elmVersion
            _ -> do
                when (isJust outputFile)
                    exitOnInputDirAndOutput

                canOverwriteFiles <- getApproval autoYes elmFiles

                if canOverwriteFiles
                    then
                        let
                            merge prev next =
                                case (prev, next) of
                                    (Nothing, Just b) -> Just b
                                    (Just b, Nothing) -> Just b
                                    (Just a, Just b) -> Just $ a && b
                                    (Nothing, Nothing) -> Nothing
                        in
                            do
                                validationResults <- mapM (\file -> processFileInput file (Just file) validateOnly elmVersion) elmFiles
                                return $ foldl merge Nothing validationResults
                    else
                        return Nothing

main :: IO ()
main =
    do
        config <- Flags.parse
        let inputFiles = (Flags._input config)
        let isStdin = (Flags._stdin config)
        let outputFile = (Flags._output config)
        let validateOnly = (Flags._validate config)
        let autoYes = validateOnly || (Flags._yes config)
        let elmVersion = Flags._elmVersion config

        let noInputSource = null inputFiles && not isStdin
        let twoInputSources = (not $ null inputFiles) && isStdin

        -- when we don't have any input, stdin or otherwise
        when (noInputSource) $ do
            Flags.showHelpText
            exitFailure

        when (twoInputSources) $ do
            exitTooManyInputSources

        when (isStdin) $ do
            handleStdinInput outputFile elmVersion
            exitSuccess

        validationResult <- handleFilesInput inputFiles outputFile autoYes validateOnly elmVersion

        case validationResult of
            Nothing ->
                exitSuccess

            Just True ->
                exitSuccess

            Just False ->
                exitFailure

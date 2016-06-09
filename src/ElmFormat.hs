{-# OPTIONS_GHC -Wall #-}
module ElmFormat where

import Elm.Utils ((|>))
import System.Exit (exitFailure, exitSuccess)
import Messages.Types
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


resolveFile :: FilePath -> IO (Either InputFileMessage [FilePath])
resolveFile path =
    do
        isFile <- Dir.doesFileExist path
        isDirectory <- Dir.doesDirectoryExist path

        case (isFile, isDirectory) of
            ( True, _ ) ->
                return $ Right [path]

            ( _, True ) ->
                do
                    elmFiles <- FS.findAllElmFiles path
                    case elmFiles of
                        [] -> return $ Left $ NoElmFiles path
                        _ -> return $ Right elmFiles

            ( False, False ) ->
                return $ Left $ FileDoesNotExist path


collectErrors :: [Either l r] -> Either [l] [r]
collectErrors list =
    let
        step acc next =
            case (next, acc) of
                (Left l, Right _) ->
                    Left [l]

                (Left l, Left ls) ->
                    Left (l : ls)

                (Right r, Right rs) ->
                    Right (r : rs)

                (Right _, Left ls) ->
                    Left ls
    in
        foldl step (Right []) list


resolveFiles :: [FilePath] -> IO (Either [InputFileMessage] [FilePath])
resolveFiles inputFiles =
    do
        result <- collectErrors <$> mapM resolveFile inputFiles
        case result of
            Left ls ->
                return $ Left ls

            Right files ->
                return $ Right $ concat files


handleFilesInput :: [FilePath] -> Maybe FilePath -> Bool -> Bool -> ElmVersion -> IO (Maybe Bool)
handleFilesInput inputFiles outputFile autoYes validateOnly elmVersion =
    do
        elmFiles <- resolveFiles inputFiles

        case elmFiles of
            Left errors ->
                do
                    putStrLn $ r $ BadInputFiles errors
                    exitFailure

            Right [inputFile] -> do
                realOutputFile <- decideOutputFile autoYes inputFile outputFile
                processFileInput inputFile (Just realOutputFile) validateOnly elmVersion

            Right elmFiles -> do
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


main :: ElmVersion -> IO ()
main defaultVersion =
    do
        config <- Flags.parse defaultVersion
        let inputFiles = (Flags._input config)
        let isStdin = (Flags._stdin config)
        let outputFile = (Flags._output config)
        let validateOnly = (Flags._validate config)
        let autoYes = validateOnly || (Flags._yes config)
        let elmVersion = Flags._elmVersion config

        let noInputSource = null inputFiles && not isStdin
        let twoInputSources = (not $ null inputFiles) && isStdin

        -- when we don't have any input, stdin or otherwise
        when (noInputSource) $
            Flags.showHelpText defaultVersion
            >> exitFailure

        when (twoInputSources) $
            exitTooManyInputSources

        when (isStdin) $
            handleStdinInput outputFile elmVersion
            >> exitSuccess

        validationResult <- handleFilesInput inputFiles outputFile autoYes validateOnly elmVersion

        case validationResult of
            Nothing ->
                exitSuccess

            Just True ->
                exitSuccess

            Just False ->
                exitFailure

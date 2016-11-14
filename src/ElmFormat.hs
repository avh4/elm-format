{-# OPTIONS_GHC -Wall #-}
module ElmFormat where

import Elm.Utils ((|>))
import System.Exit (exitFailure, exitSuccess)
import Messages.Types
import Control.Monad (when)
import Data.Maybe (isJust)
import CommandLine.Helpers
import ElmVersion


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


-- If elm-format was successful and formatted result differ
-- from original content, writes the results to the output file.
-- Otherwise, display errors and exit
writeResult
    :: ElmVersion
    -> Destination
    -> FilePath
    -> Text.Text
    -> Result.Result () Syntax.Error AST.Module.Module
    -> IO (Maybe Bool)
writeResult elmVersion destination inputFile inputText result =
    case result of
        Result.Result _ (Result.Ok modu) ->
            let
                renderedText =
                    Render.render elmVersion modu
                rendered =
                    renderedText
                        |> Text.encodeUtf8
            in
                case destination of
                    UpdateInPlace ->
                        (Char8.putStr rendered)
                        >> (return Nothing)

                    ValidateOnly ->
                        if inputText /= renderedText then
                            (putStrLn $ r $ FileWouldChange inputFile)
                            >> (return $ Just False)
                        else
                            return $ Just True

                    ToFile path ->
                        let
                            shouldWriteToFile =
                                inputFile /= path || inputText /= renderedText
                        in
                            if shouldWriteToFile then
                                (ByteString.writeFile path rendered)
                                >> (return Nothing)
                            else
                                return Nothing

        Result.Result _ (Result.Err errs) ->
            do
                showErrors inputFile (Text.unpack inputText) errs
                exitFailure


processTextInput :: ElmVersion -> Destination -> FilePath -> Text.Text -> IO (Maybe Bool)
processTextInput elmVersion destination inputFile inputText =
    Parse.parse inputText
        |> writeResult elmVersion destination inputFile inputText


processFileInput :: ElmVersion -> FilePath -> Destination -> IO (Maybe Bool)
processFileInput elmVersion inputFile destination =
    do
        inputText <- fmap Text.decodeUtf8 $ ByteString.readFile inputFile
        processTextInput elmVersion destination inputFile inputText


isEitherFileOrDirectory :: FilePath -> IO Bool
isEitherFileOrDirectory path = do
    fileExists <- Dir.doesFileExist path
    dirExists <- Dir.doesDirectoryExist path
    return $ fileExists || dirExists


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
                let destination = if validateOnly then ValidateOnly else ToFile realOutputFile
                putStrLn $ (r $ ProcessingFiles $ [inputFile])
                processFileInput elmVersion inputFile destination

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

                            dst file =
                                if validateOnly then
                                    ValidateOnly
                                else
                                    ToFile file
                        in
                            do
                                putStrLn $ (r $ ProcessingFiles elmFiles)
                                validationResults <- mapM (\file -> processFileInput elmVersion file (dst file)) elmFiles
                                return $ foldl merge Nothing validationResults
                    else
                        return Nothing


data WhatToDo
    = FormatToFile FilePath FilePath
    | StdinToFile FilePath
    | FormatInPlace FilePath [FilePath]
    | StdinToStdout
    | Validate Source


data Source
    = Stdin
    | FromFiles FilePath [FilePath]


data Destination
    = ValidateOnly
    | UpdateInPlace
    | ToFile FilePath


determineSource :: Bool -> [FilePath] -> Either Message Source
determineSource stdin inputFiles =
    case ( stdin, inputFiles ) of
        ( True, [] ) -> Right Stdin
        ( False, [] ) -> Left Error_NoInputs
        ( False, first:rest ) -> Right $ FromFiles first rest
        ( True, _:_ ) -> Left Error_TooManyInputs


determineDestination :: Maybe FilePath -> Bool -> Either Message Destination
determineDestination output validate =
    case ( output, validate ) of
        ( Nothing, True ) -> Right ValidateOnly
        ( Nothing, False ) -> Right UpdateInPlace
        ( Just path, False ) -> Right $ ToFile path
        ( Just _, True ) -> Left Error_OutputAndValidate


determineWhatToDo :: Source -> Destination -> Either Message WhatToDo
determineWhatToDo source destination =
    case ( source, destination ) of
        ( _, ValidateOnly ) -> Right $ Validate source
        ( Stdin, UpdateInPlace ) -> Right StdinToStdout
        ( Stdin, ToFile output ) -> Right $ StdinToFile output
        ( FromFiles first [], ToFile output ) -> Right $ FormatToFile first output
        ( FromFiles first rest, UpdateInPlace ) -> Right $ FormatInPlace first rest
        ( FromFiles _ _, ToFile _ ) -> Left Error_SingleOutputWithMultipleInputs


determineWhatToDoFromConfig :: Flags.Config -> Either Message WhatToDo
determineWhatToDoFromConfig config =
    do
        source <- determineSource (Flags._stdin config) (Flags._input config)
        destination <- determineDestination (Flags._output config) (Flags._validate config)
        determineWhatToDo source destination


validate :: ElmVersion -> Source -> IO ()
validate elmVersion source =
    do
        result <-
            case source of
                Stdin ->
                    do
                        input <- Lazy.getContents

                        Lazy.toStrict input
                            |> Text.decodeUtf8
                            |> processTextInput elmVersion ValidateOnly "<STDIN>"

                FromFiles first rest ->
                    handleFilesInput (first:rest) Nothing True True elmVersion

        case result of
            Nothing ->
                error "Validation should always give a result"

            Just True ->
                exitSuccess

            Just False ->
                exitFailure


exitWithError :: Message -> IO ()
exitWithError message =
    (putStrLn $ r $ message)
        >> exitFailure


determineVersion :: ElmVersion -> Bool -> Either Message ElmVersion
determineVersion elmVersion upgrade =
    case (elmVersion, upgrade) of
        (Elm_0_18, True) ->
            Right Elm_0_18_Upgrade

        (_, True) ->
            Left $ MustSpecifyVersionWithUpgrade Elm_0_18_Upgrade

        (_, False) ->
            Right elmVersion


main :: ElmVersion -> IO ()
main defaultVersion =
    do
        config <- Flags.parse defaultVersion
        let autoYes = Flags._yes config
        let elmVersionResult = determineVersion (Flags._elmVersion config) (Flags._upgrade config)

        case (elmVersionResult, determineWhatToDoFromConfig config) of
            (_, Left Error_NoInputs) ->
                Flags.showHelpText defaultVersion
                    >> exitFailure

            (_, Left message) ->
                exitWithError message

            (Left message, _) ->
                exitWithError message

            (Right elmVersion, Right (Validate source)) ->
                validate elmVersion source

            (Right elmVersion, Right (FormatInPlace first rest)) ->
                do
                    result <- handleFilesInput (first:rest) Nothing autoYes False elmVersion
                    case result of
                        Nothing ->
                            exitSuccess

                        Just _ ->
                            error "There shouldn't be a validation result when formatting"

            (Right elmVersion, Right (FormatToFile input output)) ->
                do
                    result <- handleFilesInput [input] (Just output) autoYes False elmVersion
                    case result of
                        Nothing ->
                            exitSuccess

                        Just _ ->
                            error "There shouldn't be a validation result when formatting"

            (Right elmVersion, Right (StdinToStdout)) ->
                do
                    input <- Lazy.getContents

                    result <-
                        Lazy.toStrict input
                            |> Text.decodeUtf8
                            |> processTextInput elmVersion UpdateInPlace "<STDIN>"
                    case result of
                        Nothing ->
                            exitSuccess

                        Just _ ->
                            error "There shouldn't be a validation result when formatting"

            (Right elmVersion, Right (StdinToFile output)) ->
                do
                    input <- Lazy.getContents

                    result <-
                        Lazy.toStrict input
                            |> Text.decodeUtf8
                            |> processTextInput elmVersion (ToFile output) "<STDIN>"
                    case result of
                        Nothing ->
                            exitSuccess

                        Just _ ->
                            error "There shouldn't be a validation result when formatting"

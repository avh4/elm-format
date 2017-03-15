{-# OPTIONS_GHC -Wall #-}
module ElmFormat where

import Elm.Utils ((|>))
import System.Exit (exitFailure, exitSuccess)
import Messages.Types
import Messages.Formatter.Format
import Control.Monad (when)
import Control.Monad.Free
import Data.Maybe (isJust)
import CommandLine.Helpers
import ElmVersion
import ElmFormat.FileStore (FileStore)
import ElmFormat.Operation (Operation)

import qualified AST.Module
import qualified Flags
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified ElmFormat.Execute as Execute
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.Filesystem as FS
import qualified ElmFormat.Operation as Operation
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Result as Result


-- If elm-format was successful and formatted result differ
-- from original content, writes the results to the output file.
-- Otherwise, display errors and exit
writeResult
    :: Operation f =>
    ElmVersion
    -> Destination
    -> FilePath
    -> Text.Text
    -> Result.Result () Syntax.Error AST.Module.Module
    -> Free f (Maybe Bool)
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
                        Operation.deprecatedIO $
                        Char8.putStr rendered
                        >> return Nothing

                    ValidateOnly ->
                        if inputText /= renderedText then
                            onInfo (FileWouldChange inputFile)
                            >> return (Just False)
                        else
                            return $ Just True

                    ToFile path ->
                        let
                            shouldWriteToFile =
                                inputFile /= path || inputText /= renderedText
                        in
                            if shouldWriteToFile then
                                Operation.deprecatedIO $
                                ByteString.writeFile path rendered
                                >> return Nothing
                            else
                                return Nothing

        Result.Result _ (Result.Err errs) ->
            onInfo (ParseError inputFile (Text.unpack inputText) errs)
            >> return (Just False)


processTextInput :: Operation f => ElmVersion -> Destination -> FilePath -> Text.Text -> Free f (Maybe Bool)
processTextInput elmVersion destination inputFile inputText =
    Parse.parse inputText
        |> writeResult elmVersion destination inputFile inputText


processFileInput :: Operation f => ElmVersion -> FilePath -> Destination -> Free f (Maybe Bool)
processFileInput elmVersion inputFile destination =
    do
        inputText <- Operation.deprecatedIO $ fmap Text.decodeUtf8 $ ByteString.readFile inputFile
        processTextInput elmVersion destination inputFile inputText


resolveFile :: FileStore f => FilePath -> Free f (Either InputFileMessage [FilePath])
resolveFile path =
    do
        fileType <- FileStore.stat path

        case fileType of
            FileStore.IsFile ->
                return $ Right [path]

            FileStore.IsDirectory ->
                do
                    elmFiles <- FS.findAllElmFiles path
                    case elmFiles of
                        [] -> return $ Left $ NoElmFiles path
                        _ -> return $ Right elmFiles

            FileStore.DoesNotExist ->
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


resolveFiles :: FileStore f => [FilePath] -> Free f (Either [InputFileMessage] [FilePath])
resolveFiles inputFiles =
    do
        result <- collectErrors <$> mapM resolveFile inputFiles
        case result of
            Left ls ->
                return $ Left ls

            Right files ->
                return $ Right $ concat files


handleFilesInput :: Operation f => ElmVersion -> [FilePath] -> Maybe FilePath -> Bool -> Bool -> Free f (Maybe Bool)
handleFilesInput elmVersion inputFiles outputFile autoYes validateOnly =
    do
        elmFiles <- resolveFiles inputFiles

        case elmFiles of
            Left errors ->
                Operation.deprecatedIO $
                do
                    putStrLn $ r $ BadInputFiles errors
                    exitFailure

            Right [inputFile] -> do
                realOutputFile <- decideOutputFile autoYes inputFile outputFile
                case realOutputFile of
                    Nothing ->
                        return Nothing

                    Just realOutputFile' ->
                        do
                            let destination = if validateOnly then ValidateOnly else ToFile realOutputFile'
                            onInfo $ ProcessingFiles [inputFile]
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
                                onInfo $ ProcessingFiles elmFiles
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


determineSource :: Bool -> [FilePath] -> Either ErrorMessage Source
determineSource stdin inputFiles =
    case ( stdin, inputFiles ) of
        ( True, [] ) -> Right Stdin
        ( False, [] ) -> Left NoInputs
        ( False, first:rest ) -> Right $ FromFiles first rest
        ( True, _:_ ) -> Left TooManyInputs


determineDestination :: Maybe FilePath -> Bool -> Either ErrorMessage Destination
determineDestination output validate =
    case ( output, validate ) of
        ( Nothing, True ) -> Right ValidateOnly
        ( Nothing, False ) -> Right UpdateInPlace
        ( Just path, False ) -> Right $ ToFile path
        ( Just _, True ) -> Left OutputAndValidate


determineWhatToDo :: Source -> Destination -> Either ErrorMessage WhatToDo
determineWhatToDo source destination =
    case ( source, destination ) of
        ( _, ValidateOnly ) -> Right $ Validate source
        ( Stdin, UpdateInPlace ) -> Right StdinToStdout
        ( Stdin, ToFile output ) -> Right $ StdinToFile output
        ( FromFiles first [], ToFile output ) -> Right $ FormatToFile first output
        ( FromFiles first rest, UpdateInPlace ) -> Right $ FormatInPlace first rest
        ( FromFiles _ _, ToFile _ ) -> Left SingleOutputWithMultipleInputs


determineWhatToDoFromConfig :: Flags.Config -> Either ErrorMessage WhatToDo
determineWhatToDoFromConfig config =
    do
        source <- determineSource (Flags._stdin config) (Flags._input config)
        destination <- determineDestination (Flags._output config) (Flags._validate config)
        determineWhatToDo source destination


validate :: Operation f => ElmVersion -> Source -> Free f Bool
validate elmVersion source =
    do
        result <-
            case source of
                Stdin ->
                    do
                        input <- Operation.deprecatedIO Lazy.getContents

                        Lazy.toStrict input
                            |> Text.decodeUtf8
                            |> processTextInput elmVersion ValidateOnly "<STDIN>"

                FromFiles first rest ->
                    handleFilesInput elmVersion (first:rest) Nothing True True

        case result of
            Nothing ->
                error "Validation should always give a result"

            Just isSuccess ->
                return isSuccess


exitWithError :: ErrorMessage -> IO ()
exitWithError message =
    (putStrLn $ r $ message)
        >> exitFailure


determineVersion :: ElmVersion -> Bool -> Either ErrorMessage ElmVersion
determineVersion elmVersion upgrade =
    case (elmVersion, upgrade) of
        (Elm_0_18, True) ->
            Right Elm_0_18_Upgrade

        (_, True) ->
            Left $ MustSpecifyVersionWithUpgrade Elm_0_18_Upgrade

        (_, False) ->
            Right elmVersion


exit :: Bool -> IO ()
exit True = exitSuccess
exit False = exitFailure


main :: ElmVersion -> IO ()
main defaultVersion =
    do
        config <- Flags.parse defaultVersion
        let autoYes = Flags._yes config
        let elmVersionResult = determineVersion (Flags._elmVersion config) (Flags._upgrade config)

        case (elmVersionResult, determineWhatToDoFromConfig config) of
            (_, Left NoInputs) ->
                Flags.showHelpText defaultVersion
                    >> exitFailure

            (_, Left message) ->
                exitWithError message

            (Left message, _) ->
                exitWithError message

            (Right elmVersion, Right (Validate source)) ->
                do
                    isSuccess <-
                        validate elmVersion source
                            |> Execute.run (Execute.forMachine elmVersion)
                    exit isSuccess

            (Right elmVersion, Right (FormatInPlace first rest)) ->
                do
                    result <- foldFree Execute.forHuman $ handleFilesInput elmVersion (first:rest) Nothing autoYes False
                    case result of
                        Just False ->
                            exitFailure

                        _ ->
                            exitSuccess

            (Right elmVersion, Right (FormatToFile input output)) ->
                do
                    result <- foldFree Execute.forHuman $ handleFilesInput elmVersion [input] (Just output) autoYes False
                    case result of
                        Just False ->
                            exitFailure

                        _ ->
                            exitSuccess

            (Right elmVersion, Right StdinToStdout) ->
                do
                    input <- Lazy.getContents

                    result <-
                        Lazy.toStrict input
                            |> Text.decodeUtf8
                            |> processTextInput elmVersion UpdateInPlace "<STDIN>"
                            |> foldFree Execute.forHuman
                    case result of
                        Just False ->
                            exitFailure

                        _ ->
                            exitSuccess

            (Right elmVersion, Right (StdinToFile output)) ->
                do
                    input <- Lazy.getContents

                    result <-
                        Lazy.toStrict input
                            |> Text.decodeUtf8
                            |> processTextInput elmVersion (ToFile output) "<STDIN>"
                            |> foldFree Execute.forHuman
                    case result of
                        Just False ->
                            exitFailure

                        _ ->
                            exitSuccess

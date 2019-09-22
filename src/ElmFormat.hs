{-# OPTIONS_GHC -Wall #-}
module ElmFormat (main, main') where

import Prelude hiding (putStr, putStrLn)

import System.Exit (ExitCode(..))
import Messages.Types
import Messages.Formatter.Format
import Control.Monad.Free
import qualified CommandLine.Helpers as Helpers
import ElmVersion
import ElmFormat.FileStore (FileStore)
import ElmFormat.FileWriter (FileWriter)
import ElmFormat.InputConsole (InputConsole)
import ElmFormat.OutputConsole (OutputConsole)
import ElmFormat.World

import qualified AST.Json
import qualified AST.Module
import qualified Flags
import qualified Data.Text as Text
import qualified ElmFormat.Execute as Execute
import qualified ElmFormat.InputConsole as InputConsole
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.FileWriter as FileWriter
import qualified ElmFormat.Filesystem as FS
import qualified ElmFormat.OutputConsole as OutputConsole
import qualified ElmFormat.Version
import qualified Options.Applicative as Opt
import qualified Reporting.Result as Result
import qualified Text.JSON


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


data WhatToDo
    = Format TransformMode
    | ConvertToJson TransformMode
    | ValidateStdin
    | ValidateFiles FilePath [FilePath]


data Source
    = Stdin
    | FromFiles FilePath [FilePath]


data Destination
    = InPlace
    | ToFile FilePath


data Mode
    = FormatMode
    | JsonMode
    | ValidateMode


determineSource :: Bool -> Either [InputFileMessage] [FilePath] -> Either ErrorMessage Source
determineSource stdin inputFiles =
    case ( stdin, inputFiles ) of
        ( _, Left fileErrors ) -> Left $ BadInputFiles fileErrors
        ( True, Right [] ) -> Right Stdin
        ( False, Right [] ) -> Left NoInputs
        ( False, Right (first:rest) ) -> Right $ FromFiles first rest
        ( True, Right (_:_) ) -> Left TooManyInputs


determineDestination :: Maybe FilePath -> Either ErrorMessage Destination
determineDestination output =
    case output of
        Nothing -> Right InPlace
        Just path -> Right $ ToFile path


determineMode :: Bool -> Bool -> Either ErrorMessage Mode
determineMode doValidate json =
    case ( doValidate, json ) of
        ( False, False ) -> Right FormatMode
        ( True, False ) -> Right ValidateMode
        ( False, True ) -> Right JsonMode
        _ -> Left OutputAndValidate


determineWhatToDo :: Source -> Destination -> Mode -> Either ErrorMessage WhatToDo
determineWhatToDo source destination mode =
    case ( mode, source, destination ) of
        ( ValidateMode, _, ToFile _) -> Left OutputAndValidate
        ( ValidateMode, Stdin, _ ) -> Right $ ValidateStdin
        ( ValidateMode, FromFiles first rest, _) -> Right $ ValidateFiles first rest
        ( FormatMode, Stdin, InPlace ) -> Right $ Format StdinToStdout
        ( FormatMode, Stdin, ToFile output ) -> Right $ Format (StdinToFile output)
        ( FormatMode, FromFiles first [], ToFile output ) -> Right $ Format (FileToFile first output)
        ( FormatMode, FromFiles first rest, InPlace ) -> Right $ Format (FilesInPlace first rest)
        ( JsonMode, Stdin, InPlace ) -> Right $ ConvertToJson StdinToStdout
        ( JsonMode, Stdin, ToFile output ) -> Right $ ConvertToJson (StdinToFile output)
        ( JsonMode, FromFiles first [], InPlace ) -> Right $ ConvertToJson (FileToStdout first)
        ( JsonMode, FromFiles _ (_:_), _ ) -> error "TODO: --json with multiple files is not supported"
        ( _, FromFiles _ _, ToFile _ ) -> Left SingleOutputWithMultipleInputs


determineWhatToDoFromConfig :: Flags.Config -> Either [InputFileMessage] [FilePath] -> Either ErrorMessage WhatToDo
determineWhatToDoFromConfig config resolvedInputFiles =
    do
        source <- determineSource (Flags._stdin config) resolvedInputFiles
        destination <- determineDestination (Flags._output config)
        mode <- determineMode (Flags._validate config) (Flags._json config)
        determineWhatToDo source destination mode


exitWithError :: World m => ErrorMessage -> m ()
exitWithError message =
    (putStrLnStderr $ Helpers.r $ message)
        >> exitFailure


determineVersion :: ElmVersion -> Bool -> Either ErrorMessage ElmVersion
determineVersion elmVersion upgrade =
    case (elmVersion, upgrade) of
        (Elm_0_18, True) ->
            Right Elm_0_18_Upgrade

        (Elm_0_19, True) ->
            Right Elm_0_19_Upgrade

        (_, True) ->
            Left $ MustSpecifyVersionWithUpgrade Elm_0_19_Upgrade

        (_, False) ->
            Right elmVersion


elmFormatVersion :: String
elmFormatVersion =
    ElmFormat.Version.asString


experimental :: Maybe String
experimental =
    ElmFormat.Version.experimental


{-| copied from Options.Applicative -}
handleParseResult :: World m => Opt.ParserResult a -> m (Maybe a)
handleParseResult (Opt.Success a) = return (Just a)
handleParseResult (Opt.Failure failure) = do
      progn <- getProgName
      let (msg, exit) = Opt.renderFailure failure progn
      case exit of
        ExitSuccess -> putStrLn msg *> exitSuccess *> return Nothing
        _           -> putStrLnStderr msg *> exitFailure *> return Nothing
handleParseResult (Opt.CompletionInvoked _) = do
      -- progn <- getProgName
      -- msg <- Opt.execCompletion compl progn
      -- putStr msg
      -- const undefined <$> exitSuccess
      error "Shell completion not yet implemented"


main :: World m => [String] -> m ()
main args =
    main' elmFormatVersion experimental args

main' :: World m => String -> Maybe String -> [String] -> m ()
main' elmFormatVersion_ experimental_ args =
    do
        c <- handleParseResult $ Flags.parse elmFormatVersion_ experimental_ args
        case c of
            Nothing -> return ()
            Just config ->
                do
                    let autoYes = Flags._yes config
                    resolvedInputFiles <- Execute.run (Execute.forHuman autoYes) $ resolveFiles (Flags._input config)

                    case determineWhatToDoFromConfig config resolvedInputFiles of
                        Left NoInputs ->
                            (handleParseResult $ Flags.showHelpText elmFormatVersion_ experimental_)
                                -- TODO: handleParseResult is exitSuccess, so we never get to exitFailure
                                >> exitFailure

                        Left message ->
                            exitWithError message

                        Right whatToDo -> do
                            elmVersionChoice <- case Flags._elmVersion config of
                                Just v -> return $ Right v
                                Nothing -> autoDetectElmVersion

                            case elmVersionChoice of
                                Left message ->
                                    putStr message *> exitFailure

                                Right elmVersionChoice' -> do
                                    let elmVersionResult = determineVersion elmVersionChoice' (Flags._upgrade config)

                                    case elmVersionResult of
                                        Left message ->
                                            exitWithError message

                                        Right elmVersion ->
                                            do
                                                let run = case (Flags._validate config) of
                                                        True -> Execute.run $ Execute.forMachine elmVersion True
                                                        False -> Execute.run $ Execute.forHuman autoYes
                                                result <-  run $ doIt elmVersion whatToDo
                                                if result
                                                    then exitSuccess
                                                    else exitFailure


autoDetectElmVersion :: World m => m (Either String ElmVersion)
autoDetectElmVersion =
    do
        hasElmPackageJson <- doesFileExist "elm-package.json"
        if hasElmPackageJson
            then
                do
                    hasElmJson <- doesFileExist "elm.json"
                    if hasElmJson
                        then return $ Right Elm_0_19
                        else return $ Right Elm_0_18
            else return $ Right Elm_0_19


validate :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage ()
validate elmVersion (inputFile, inputText) =
    case Parse.parse elmVersion inputText of
        Result.Result _ (Result.Ok modu) ->
            if inputText /= Render.render elmVersion modu then
                Left $ FileWouldChange inputFile
            else
                Right ()

        Result.Result _ (Result.Err errs) ->
            Left $ ParseError inputFile (Text.unpack inputText) errs


data FormatResult
    = NoChange FilePath Text.Text
    | Changed FilePath Text.Text


parseModule :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage AST.Module.Module
parseModule elmVersion (inputFile, inputText) =
    case Parse.parse elmVersion inputText of
        Result.Result _ (Result.Ok modu) ->
            Right modu

        Result.Result _ (Result.Err errs) ->
            Left $ ParseError inputFile (Text.unpack inputText) errs


checkChange :: (FilePath, Text.Text) -> Text.Text -> FormatResult
checkChange (inputFile, inputText) outputText =
    if inputText == outputText
        then NoChange inputFile outputText
        else Changed inputFile outputText


format :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage FormatResult
format elmVersion input =
    checkChange input <$> Render.render elmVersion <$> parseModule elmVersion input


toJson :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage FormatResult
toJson elmVersion (inputFile, inputText) =
    Changed inputFile . Text.pack . Text.JSON.encode . AST.Json.showModule
    <$> parseModule elmVersion (inputFile, inputText)


readStdin :: InputConsole f => Free f (FilePath, Text.Text)
readStdin =
    (,) "<STDIN>" <$> InputConsole.readStdin


readFile :: (FileStore f, InfoFormatter f) => FilePath -> Free f (FilePath, Text.Text)
readFile filePath =
    onInfo (ProcessingFile filePath)
        *> ((,) filePath <$> FileStore.readFile filePath)


getOutputText :: FormatResult -> Text.Text
getOutputText result =
    case result of
        NoChange _ text -> text
        Changed _ text -> text


updateFile :: FileWriter f => FormatResult -> Free f ()
updateFile result =
    case result of
        NoChange _ _ -> return ()
        Changed outputFile outputText -> FileWriter.overwriteFile outputFile outputText


logError :: InfoFormatter f => Either InfoMessage () -> Free f Bool
logError result =
    case result of
        Left message ->
            onInfo message *> return False

        Right () ->
            return True


logErrorOr :: InfoFormatter f => (a -> Free f ()) -> Either InfoMessage a -> Free f Bool
logErrorOr fn result =
    case result of
        Left message ->
            onInfo message *> return False

        Right value ->
            fn value *> return True


data TransformMode
    = StdinToStdout
    | StdinToFile FilePath
    | FileToStdout FilePath
    | FileToFile FilePath FilePath
    | FilesInPlace FilePath [FilePath]


applyTransformation :: (InputConsole f, OutputConsole f, InfoFormatter f, FileStore f, FileWriter f) => ((FilePath, Text.Text) -> Either InfoMessage FormatResult) -> TransformMode -> Free f Bool
applyTransformation transform mode =
    case mode of
        StdinToStdout ->
            (fmap getOutputText <$> transform <$> readStdin) >>= logErrorOr OutputConsole.writeStdout

        StdinToFile outputFile ->
            (fmap getOutputText <$> transform <$> readStdin) >>= logErrorOr (FileWriter.overwriteFile outputFile)

        -- TODO: this prints "Processing such-and-such-a-file.elm" which makes the stdout invalid
        -- FileToStdout inputFile ->
        --     (fmap getOutputText <$> transform <$> ElmFormat.readFile inputFile) >>= logErrorOr OutputConsole.writeStdout

        FileToFile inputFile outputFile ->
            (fmap getOutputText <$> transform <$> ElmFormat.readFile inputFile) >>= logErrorOr (FileWriter.overwriteFile outputFile)

        FilesInPlace first rest ->
            do
                canOverwrite <- approve $ FilesWillBeOverwritten (first:rest)
                if canOverwrite
                    then all id <$> mapM formatFile (first:rest)
                    else return True
            where
                formatFile file = (transform <$> ElmFormat.readFile file) >>= logErrorOr ElmFormat.updateFile


doIt :: (InputConsole f, OutputConsole f, InfoFormatter f, FileStore f, FileWriter f) => ElmVersion -> WhatToDo -> Free f Bool
doIt elmVersion whatToDo =
    case whatToDo of
        ValidateStdin ->
            (validate elmVersion <$> readStdin) >>= logError

        ValidateFiles first rest ->
            all id <$> mapM validateFile (first:rest)
            where validateFile file = (validate elmVersion <$> ElmFormat.readFile file) >>= logError

        Format transformMode ->
            applyTransformation (format elmVersion) transformMode

        ConvertToJson transformMode ->
            applyTransformation (toJson elmVersion) transformMode

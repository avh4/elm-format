{-# OPTIONS_GHC -Wall #-}
module ElmFormat where

import Prelude hiding (putStr, putStrLn)

import System.Exit (ExitCode(..))
import System.Environment (getArgs)
import Messages.Types
import Messages.Formatter.Format
import Control.Monad.Free
import qualified CommandLine.Helpers as Helpers
import ElmVersion
import ElmFormat.FileStore (FileStore)
import ElmFormat.Filesystem (ElmFile)
import ElmFormat.FileWriter (FileWriter)
import ElmFormat.InputConsole (InputConsole)
import ElmFormat.OutputConsole (OutputConsole)
import ElmFormat.World

import qualified AST.Json
import qualified AST.Module
import qualified Flags
import qualified Data.Maybe as Maybe
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


resolveFile :: FileStore f => ElmVersion -> FilePath -> Free f (Either InputFileMessage [ElmFile])
resolveFile defaultElmVersion path =
    do
        upwardElmVersion <- FS.findElmVersion path
        let elmFile = FS.ElmFile (Maybe.fromMaybe defaultElmVersion upwardElmVersion) path

        fileType <- FileStore.stat path
        case fileType of
            FileStore.IsFile ->
                do
                    return $ Right [elmFile]

            FileStore.IsDirectory ->
                do
                    elmFiles <- FS.findAllElmFiles elmFile
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


resolveFiles :: FileStore f => ElmVersion -> [FilePath] -> Free f (Either [InputFileMessage] [ElmFile])
resolveFiles defaultElmVersion inputFiles =
    do
        result <- collectErrors <$> mapM (resolveFile defaultElmVersion) inputFiles
        case result of
            Left ls ->
                return $ Left ls

            Right files ->
                return $ Right $ concat $ files


data WhatToDo
    = FormatToFile ElmFile FilePath
    | StdinToFile ElmVersion FilePath
    | FormatInPlace ElmFile [ElmFile]
    | StdinToStdout ElmVersion
    | ValidateStdin ElmVersion
    | ValidateFiles ElmFile [ElmFile]
    | FileToJson ElmFile
    | StdinToJson ElmVersion


data Source
    = Stdin ElmVersion
    | FromFiles ElmFile [ElmFile]


data Destination
    = ValidateOnly
    | UpdateInPlace
    | ToFile FilePath
    | ToJson


determineSource :: Bool -> Bool -> Maybe ElmVersion -> ElmVersion -> Either [InputFileMessage] [ElmFile] -> Either ErrorMessage Source
determineSource stdin upgrade versionFlag defaultElmVersion inputFiles =
    let
        determineFile (FS.ElmFile fileDetectedVersion path) =
            FS.ElmFile (upgradeVersion upgrade $ Maybe.fromMaybe fileDetectedVersion versionFlag) path
    in
    case ( stdin, inputFiles ) of
        ( _, Left fileErrors ) -> Left $ BadInputFiles fileErrors
        ( True, Right [] ) -> Right $ Stdin $ upgradeVersion upgrade $ Maybe.fromMaybe defaultElmVersion versionFlag
        ( False, Right [] ) -> Left NoInputs
        ( False, Right (first:rest) ) -> Right $ FromFiles (determineFile first) (fmap determineFile rest)
        ( True, Right (_:_) ) -> Left TooManyInputs


upgradeVersion :: Bool -> ElmVersion -> ElmVersion
upgradeVersion upgrade version =
    case (upgrade, version) of
        (True, Elm_0_18) ->
            Elm_0_18_Upgrade

        (True, _) ->
            Elm_0_19_Upgrade

        _ ->
            version


determineDestination :: Maybe FilePath -> Bool -> Bool -> Either ErrorMessage Destination
determineDestination output doValidate json =
    case ( output, doValidate, json ) of
        ( _, True, True ) -> Left OutputAndValidate
        ( Nothing, True, False ) -> Right ValidateOnly
        ( Nothing, False, False ) -> Right UpdateInPlace
        ( Just path, False, False ) -> Right $ ToFile path
        ( Just _, True, _ ) -> Left OutputAndValidate
        ( _, False, True ) -> Right ToJson


determineWhatToDo :: Source -> Destination -> Either ErrorMessage WhatToDo
determineWhatToDo source destination =
    case ( source, destination ) of
        ( Stdin version, ValidateOnly ) -> Right $ ValidateStdin version
        ( FromFiles first rest, ValidateOnly) -> Right $ ValidateFiles first rest
        ( Stdin version, UpdateInPlace ) -> Right $ StdinToStdout version
        ( Stdin version, ToJson ) -> Right $ StdinToJson version
        ( Stdin version, ToFile output ) -> Right $ StdinToFile version output
        ( FromFiles first [], ToFile output ) -> Right $ FormatToFile first output
        ( FromFiles first rest, UpdateInPlace ) -> Right $ FormatInPlace first rest
        ( FromFiles _ _, ToFile _ ) -> Left SingleOutputWithMultipleInputs
        ( FromFiles first [], ToJson ) -> Right $ FileToJson first
        ( FromFiles _ _, ToJson ) -> Left SingleOutputWithMultipleInputs


determineWhatToDoFromConfig :: Flags.Config -> ElmVersion -> Either [InputFileMessage] [ElmFile] -> Either ErrorMessage WhatToDo
determineWhatToDoFromConfig config defaultElmVersion resolvedInputFiles =
    do
        checkUpgradeVersion (Flags._upgrade config) (Flags._elmVersion config)
        source <- determineSource (Flags._stdin config) (Flags._upgrade config) (Flags._elmVersion config) defaultElmVersion resolvedInputFiles
        destination <- determineDestination (Flags._output config) (Flags._validate config) (Flags._json config)
        determineWhatToDo source destination


checkUpgradeVersion :: Bool -> Maybe ElmVersion -> Either ErrorMessage ()
checkUpgradeVersion upgrade elmVersionFlag =
    case (upgrade, elmVersionFlag) of
        (True, Just Elm_0_18) -> Right ()
        (True, Just Elm_0_19) -> Right ()
        (True, _) -> Left $ MustSpecifyVersionWithUpgrade Elm_0_19_Upgrade
        (False, _) -> Right ()


exitWithError :: World m => ErrorMessage -> m ()
exitWithError message =
    (putStrLnStderr $ Helpers.r $ message)
        >> exitFailure


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


main :: IO ()
main =
    do
        args <- getArgs
        main' args


main' :: World m => [String] -> m ()
main' args =
    main'' elmFormatVersion experimental args

main'' :: World m => String -> Maybe String -> [String] -> m ()
main'' elmFormatVersion_ experimental_ args =
    do
        c <- handleParseResult $ Flags.parse elmFormatVersion_ experimental_ args
        case c of
            Nothing -> return ()
            Just config ->
                do
                    let autoYes = Flags._yes config
                    currentDirectoryElmVersion <- Execute.run (Execute.forHuman autoYes) $ FS.findElmVersion "."
                    let defaultElmVersion = Maybe.fromMaybe Elm_0_19 currentDirectoryElmVersion;
                    resolvedInputFiles <- Execute.run (Execute.forHuman autoYes) $ resolveFiles defaultElmVersion (Flags._input config)

                    case determineWhatToDoFromConfig config defaultElmVersion resolvedInputFiles of
                        Left NoInputs ->
                            (handleParseResult $ Flags.showHelpText elmFormatVersion_ experimental_)
                                -- TODO: handleParseResult is exitSuccess, so we never get to exitFailure
                                >> exitFailure

                        Left message ->
                            exitWithError message

                        Right whatToDo ->
                            do
                                let run = case (Flags._validate config) of
                                        True -> Execute.run $ Execute.forMachine True
                                        False -> Execute.run $ Execute.forHuman autoYes
                                result <-  run $ doIt whatToDo
                                if result
                                    then exitSuccess
                                    else exitFailure


validate :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage ()
validate elmVersion (inputFile, inputText) =
    case Parse.parse elmVersion inputText of
        Result.Result _ (Result.Ok modu) ->
            if inputText /= Render.render elmVersion modu then
                Left $ FileWouldChange inputFile elmVersion
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


format :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage FormatResult
format elmVersion (inputFile, inputText) =
    case parseModule elmVersion (inputFile, inputText) of
        Right modu ->
            let
                outputText = Render.render elmVersion modu
            in
            Right $
                if inputText == outputText
                    then NoChange inputFile outputText
                    else Changed inputFile outputText

        Left message ->
            Left message


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



doIt :: (InputConsole f, OutputConsole f, InfoFormatter f, FileStore f, FileWriter f) => WhatToDo -> Free f Bool
doIt whatToDo =
    case whatToDo of
        ValidateStdin elmVersion ->
            (validate elmVersion <$> readStdin) >>= logError

        ValidateFiles first rest ->
            all id <$> mapM validateFile (first:rest)
            where validateFile (FS.ElmFile elmVersion path) = (validate elmVersion <$> ElmFormat.readFile path) >>= logError

        StdinToStdout elmVersion ->
            (fmap getOutputText <$> format elmVersion <$> readStdin) >>= logErrorOr OutputConsole.writeStdout

        StdinToFile elmVersion outputFile ->
            (fmap getOutputText <$> format elmVersion <$> readStdin) >>= logErrorOr (FileWriter.overwriteFile outputFile)

        FormatToFile (FS.ElmFile elmVersion inputFile) outputFile ->
            (fmap getOutputText <$> format elmVersion <$> ElmFormat.readFile inputFile) >>= logErrorOr (FileWriter.overwriteFile outputFile)

        FormatInPlace first rest ->
            do
                canOverwrite <- approve $ FilesWillBeOverwritten $ fmap FS.path (first:rest)
                if canOverwrite
                    then all id <$> mapM formatFile (first:rest)
                    else return True
            where
                formatFile (FS.ElmFile elmVersion path) = (format elmVersion <$> ElmFormat.readFile path) >>= logErrorOr ElmFormat.updateFile

        StdinToJson elmVersion ->
            (fmap (Text.pack . Text.JSON.encode . AST.Json.showModule) <$> parseModule elmVersion <$> readStdin) >>= logErrorOr OutputConsole.writeStdout

        -- TODO: this prints "Processing such-and-such-a-file.elm" which makes the JSON output invalid
        -- FileToJson inputFile ->
        --     (fmap (Text.pack . Text.JSON.encode . AST.Json.showJSON) <$> parseModule <$> ElmFormat.readFile inputFile) >>= logErrorOr OutputConsole.writeStdout

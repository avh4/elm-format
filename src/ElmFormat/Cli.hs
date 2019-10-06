module ElmFormat.Cli (main, main') where

import Prelude ()
import Relude hiding (exitFailure, exitSuccess, putStr, putStrLn)

import Messages.Types
import Messages.Formatter.Format
import CommandLine.Program (ProgramIO)
import CommandLine.ResolveFiles (ResolveFileError)
import CommandLine.TransformFiles (TransformMode(..))
import Control.Monad.Free
import ElmVersion
import ElmFormat.FileStore (FileStore)
import ElmFormat.FileWriter (FileWriter)
import ElmFormat.InputConsole (InputConsole)
import ElmFormat.OutputConsole (OutputConsole)
import ElmFormat.World

import qualified AST.Json
import qualified AST.Module
import qualified CommandLine.Helpers as Helpers
import qualified CommandLine.Program as Program
import qualified CommandLine.ResolveFiles as ResolveFiles
import qualified CommandLine.TransformFiles as TransformFiles
import qualified Data.Text as Text
import qualified ElmFormat.CliFlags as Flags
import qualified ElmFormat.Execute as Execute
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmFormat.Version
import qualified Reporting.Result as Result
import qualified Text.JSON


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


determineSource :: Bool -> Either [ResolveFileError] [FilePath] -> Either ErrorMessage Source
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
        ( ValidateMode, Stdin, _ ) -> Right ValidateStdin
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


determineWhatToDoFromConfig :: Flags.Config -> Either [ResolveFileError] [FilePath] -> Either ErrorMessage WhatToDo
determineWhatToDoFromConfig config resolvedInputFiles =
    do
        source <- determineSource (Flags._stdin config) resolvedInputFiles
        destination <- determineDestination (Flags._output config)
        mode <- determineMode (Flags._validate config) (Flags._json config)
        determineWhatToDo source destination mode


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


main :: World m => [String] -> m ()
main args =
    main' elmFormatVersion experimental args

main' :: World m => String -> Maybe String -> [String] -> m ()
main' elmFormatVersion_ experimental_ args =
    Program.run (Flags.parser elmFormatVersion_ experimental_) Helpers.r run' args
    where
        run' :: World m => Flags.Config -> ProgramIO m ErrorMessage ()
        run' flags =
            do
                let autoYes = Flags._yes flags
                resolvedInputFiles <- Program.liftM $ Execute.run (Execute.forHuman autoYes) $ ResolveFiles.resolveElmFiles (Flags._input flags)

                whatToDo <- case determineWhatToDoFromConfig flags resolvedInputFiles of
                    Left NoInputs -> Program.showUsage
                    Left err -> Program.error err
                    Right a -> return a

                elmVersionChoice <- case Flags._elmVersion flags of
                    Just v -> return v
                    Nothing -> Program.liftME autoDetectElmVersion

                elmVersion <- Program.liftEither $ determineVersion elmVersionChoice (Flags._upgrade flags)

                let run = case Flags._validate flags of
                        True -> Execute.run $ Execute.forMachine elmVersion True
                        False -> Execute.run $ Execute.forHuman autoYes
                result <- Program.liftM $ run $ doIt elmVersion whatToDo
                if result
                    then return ()
                    else Program.failed


autoDetectElmVersion :: World m => m (Either x ElmVersion)
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
            Left $ ParseError inputFile (toString inputText) errs


parseModule :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage AST.Module.Module
parseModule elmVersion (inputFile, inputText) =
    case Parse.parse elmVersion inputText of
        Result.Result _ (Result.Ok modu) ->
            Right modu

        Result.Result _ (Result.Err errs) ->
            Left $ ParseError inputFile (toString inputText) errs


format :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage Text.Text
format elmVersion input =
    Render.render elmVersion <$> parseModule elmVersion input


toJson :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage Text.Text
toJson elmVersion (inputFile, inputText) =
    toText . Text.JSON.encode . AST.Json.showModule
    <$> parseModule elmVersion (inputFile, inputText)


logError :: InfoFormatter f => Either InfoMessage () -> Free f Bool
logError result =
    case result of
        Left message ->
            onInfo message *> return False

        Right () ->
            return True


doIt :: (InputConsole f, OutputConsole f, InfoFormatter f, FileStore f, FileWriter f) => ElmVersion -> WhatToDo -> Free f Bool
doIt elmVersion whatToDo =
    case whatToDo of
        ValidateStdin ->
            (validate elmVersion <$> TransformFiles.readStdin) >>= logError

        ValidateFiles first rest ->
            and <$> mapM validateFile (first:rest)
            where validateFile file = (validate elmVersion <$> TransformFiles.readFromFile file) >>= logError

        Format transformMode ->
            TransformFiles.applyTransformation (format elmVersion) transformMode

        ConvertToJson transformMode ->
            TransformFiles.applyTransformation (toJson elmVersion) transformMode

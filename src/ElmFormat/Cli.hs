{-# LANGUAGE DataKinds #-}
module ElmFormat.Cli (main, main') where

import Prelude ()
import Relude hiding (exitFailure, exitSuccess, putStr, putStrLn)

import AST.Module (Module)
import AST.Structure
import AST.V0_16
import CommandLine.Program (ProgramIO)
import CommandLine.TransformFiles (TransformMode(..), ValidateMode(..))
import CommandLine.World
import ElmFormat.Messages
import ElmVersion
import Reporting.Annotation (Located)

import qualified CommandLine.Program as Program
import qualified CommandLine.ResolveFiles as ResolveFiles
import qualified CommandLine.TransformFiles as TransformFiles
import qualified Data.Text as Text
import qualified ElmFormat.CliFlags as Flags
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmFormat.Version
import qualified Reporting.Result as Result
import qualified ElmFormat.AST.PublicAST as PublicAST
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB


data WhatToDo
    = Format TransformMode
    | ConvertToJson TransformMode
    | ConvertFromJson TransformMode
    | Validate ValidateMode


data Source
    = Stdin
    | FromFiles FilePath [FilePath]


data Destination
    = InPlace
    | ToFile FilePath


data Mode
    = FormatMode
    | JsonMode Flags.JsonMode
    | ValidateMode


determineSource :: Bool -> Either [ResolveFiles.Error] [FilePath] -> Either ErrorMessage Source
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


determineMode :: Bool -> Maybe Flags.JsonMode -> Either ErrorMessage Mode
determineMode doValidate json =
    case ( doValidate, json ) of
        ( False, Nothing ) -> Right FormatMode
        ( True, Nothing ) -> Right ValidateMode
        ( False, Just jsonMode ) -> Right (JsonMode jsonMode)
        _ -> Left OutputAndValidate


determineWhatToDo :: Source -> Destination -> Mode -> Either ErrorMessage WhatToDo
determineWhatToDo source destination mode =
    case ( mode, source, destination ) of
        ( ValidateMode, _, ToFile _) -> Left OutputAndValidate
        ( ValidateMode, Stdin, _ ) -> Right $ Validate ValidateStdin
        ( ValidateMode, FromFiles first rest, _) -> Right $ Validate (ValidateFiles first rest)
        ( FormatMode, Stdin, InPlace ) -> Right $ Format StdinToStdout
        ( FormatMode, Stdin, ToFile output ) -> Right $ Format (StdinToFile output)
        ( FormatMode, FromFiles first [], ToFile output ) -> Right $ Format (FileToFile first output)
        ( FormatMode, FromFiles first rest, InPlace ) -> Right $ Format (FilesInPlace first rest)
        ( JsonMode Flags.ElmToJson, Stdin, InPlace ) -> Right $ ConvertToJson StdinToStdout
        ( JsonMode Flags.ElmToJson, Stdin, ToFile output ) -> Right $ ConvertToJson (StdinToFile output)
        ( JsonMode Flags.ElmToJson, FromFiles first [], InPlace ) -> Right $ ConvertToJson (FileToStdout first)
        ( JsonMode Flags.ElmToJson, FromFiles _ (_:_), _ ) -> error "TODO: --json with multiple files is not supported"
        ( JsonMode Flags.JsonToElm, Stdin, InPlace ) -> Right $ ConvertFromJson StdinToStdout
        ( JsonMode Flags.JsonToElm, Stdin, ToFile output ) -> Right $ ConvertFromJson (StdinToFile output)
        ( JsonMode Flags.JsonToElm, FromFiles first [], InPlace ) -> Right $ ConvertFromJson (FileToStdout first)
        ( JsonMode Flags.JsonToElm, FromFiles _ (_:_), _ ) -> error "TODO: --from-json with multiple files is not supported"
        ( _, FromFiles _ _, ToFile _ ) -> Left SingleOutputWithMultipleInputs


determineWhatToDoFromConfig :: Flags.Config -> Either [ResolveFiles.Error] [FilePath] -> Either ErrorMessage WhatToDo
determineWhatToDoFromConfig config resolvedInputFiles =
    do
        source <- determineSource (Flags._stdin config) resolvedInputFiles
        destination <- determineDestination (Flags._output config)
        mode <- determineMode (Flags._validate config) (Flags._json config)
        determineWhatToDo source destination mode


main :: World m => [String] -> m ()
main =
    main'
        ElmFormat.Version.asString
        ElmFormat.Version.experimental

main' :: World m => String -> Maybe String -> [String] -> m ()
main' elmFormatVersion experimental args =
    Program.run (Flags.parser elmFormatVersion experimental) run' args
    where
        run' :: World m => Flags.Config -> ProgramIO m ErrorMessage ()
        run' flags =
            do
                resolvedInputFiles <- Program.liftM $ ResolveFiles.resolveElmFiles (Flags._input flags)

                whatToDo <- case determineWhatToDoFromConfig flags resolvedInputFiles of
                    Left NoInputs -> Program.showUsage
                    Left err -> Program.error err
                    Right a -> return a

                elmVersion <- case Flags._elmVersion flags of
                    Just v -> return v
                    Nothing -> Program.liftME autoDetectElmVersion

                let autoYes = Flags._yes flags
                result <- Program.liftM $ doIt elmVersion autoYes whatToDo
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
validate elmVersion input@(inputFile, inputText) =
    case parseModule elmVersion input of
        Right modu ->
            if inputText /= Render.render elmVersion modu then
                Left $ FileWouldChange elmVersion inputFile
            else
                Right ()

        Left err ->
            Left err


parseModule ::
    ElmVersion
    -> (FilePath, Text.Text)
    -> Either InfoMessage (Module [UppercaseIdentifier] (ASTNS Located [UppercaseIdentifier] 'TopLevelNK))
parseModule elmVersion (inputFile, inputText) =
    case Parse.parse elmVersion inputText of
        Result.Result _ (Result.Ok modu) ->
            Right modu

        Result.Result _ (Result.Err errs) ->
            Left $ ParseError inputFile errs


parseJson :: (FilePath, Text.Text)
    -> Either InfoMessage (Module [UppercaseIdentifier] (ASTNS Identity [UppercaseIdentifier] 'TopLevelNK))
parseJson (inputFile, inputText) =
    case Aeson.eitherDecode (LB.fromChunks . return . encodeUtf8 $ inputText) of
        Right modu -> Right $ PublicAST.toModule modu

        Left message ->
            Left $ JsonParseError inputFile (Text.pack message)


format :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage Text.Text
format elmVersion input =
    Render.render elmVersion <$> parseModule elmVersion input


toJson :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage Text.Text
toJson elmVersion (inputFile, inputText) =
    let
        config =
            PublicAST.Config
                { PublicAST.showSourceLocation = True
                }
    in
    decodeUtf8 . B.concat . LB.toChunks . Aeson.encode . PublicAST.fromModule config
    <$> parseModule elmVersion (inputFile, inputText)


fromJson :: ElmVersion -> (FilePath, Text.Text) -> Either InfoMessage Text.Text
fromJson elmVersion input =
    Render.render elmVersion <$> parseJson input


doIt :: World m => ElmVersion -> Bool -> WhatToDo -> m Bool
doIt elmVersion autoYes whatToDo =
    case whatToDo of
        Validate validateMode ->
            TransformFiles.validateNoChanges
                ProcessingFile
                (validate elmVersion)
                validateMode

        Format transformMode ->
            TransformFiles.applyTransformation
                ProcessingFile autoYes FilesWillBeOverwritten
                (format elmVersion)
                transformMode

        ConvertToJson transformMode ->
            TransformFiles.applyTransformation
                ProcessingFile autoYes FilesWillBeOverwritten
                (toJson elmVersion)
                transformMode

        ConvertFromJson transformMode ->
            TransformFiles.applyTransformation
                ProcessingFile autoYes FilesWillBeOverwritten
                (fromJson elmVersion)
                transformMode

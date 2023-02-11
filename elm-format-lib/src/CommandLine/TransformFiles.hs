module CommandLine.TransformFiles
    ( Result
    , TransformMode(..), applyTransformation
    , ValidateMode(..), validateNoChanges
    ) where

-- This module provides reusable functions for command line tools that
-- transform files.

import qualified CommandLine.InfoFormatter as InfoFormatter
import CommandLine.World (World)
import qualified CommandLine.World as World
import qualified Data.Either as Either
import Data.Text (Text)



data Result a
    = NoChange FilePath a
    | Changed FilePath a


checkChange :: Eq a => (FilePath, a) -> a -> Result a
checkChange (inputFile, inputText) outputText =
    if inputText == outputText
        then NoChange inputFile outputText
        else Changed inputFile outputText


updateFile :: World m => Result Text -> m ()
updateFile result =
    case result of
        NoChange _ _ -> return ()
        Changed outputFile outputText -> World.writeUtf8File outputFile outputText


readStdin :: World m => m (FilePath, Text)
readStdin =
    (,) "<STDIN>" <$> World.getStdin


readFromFile :: World m => (FilePath -> m ()) -> FilePath -> m (FilePath, Text)
readFromFile onProcessingFile filePath =
    do
        onProcessingFile filePath
        World.readUtf8FileWithPath filePath


data TransformMode
    = StdinToStdout
    | StdinToFile FilePath
    | FileToStdout FilePath
    | FileToFile FilePath FilePath
    | FilesInPlace FilePath [FilePath]


applyTransformation ::
    World m =>
    InfoFormatter.Loggable info =>
    InfoFormatter.ToConsole prompt =>
    (FilePath -> info)
    -> Bool
    -> ([FilePath] -> prompt)
    -> ((FilePath, Text) -> Either info Text)
    -> TransformMode
    -> m Bool
applyTransformation processingFile autoYes confirmPrompt transform mode =
    let
        usesStdout =
            case mode of
                StdinToStdout -> True
                StdinToFile _ -> True
                FileToStdout _ -> True
                FileToFile _ _ -> False
                FilesInPlace _ _ -> False

        onInfo info = InfoFormatter.putStrLn' usesStdout (InfoFormatter.toConsole info)

    in
    case mode of
        StdinToStdout ->
            readStdin >>= logErrorOr onInfo World.writeStdout . transform

        StdinToFile outputFile ->
            readStdin >>= logErrorOr onInfo (World.writeUtf8File outputFile) . transform

        FileToStdout inputFile ->
            World.readUtf8FileWithPath inputFile >>= logErrorOr onInfo World.writeStdout . transform

        FileToFile inputFile outputFile ->
            readFromFile (onInfo . processingFile) inputFile >>= logErrorOr onInfo (World.writeUtf8File outputFile) . transform

        FilesInPlace first rest ->
            do
                canOverwrite <- InfoFormatter.approve autoYes $ confirmPrompt $ first:rest
                if canOverwrite
                    then and <$> World.mapMConcurrently formatFile (first:rest)
                    else return True
            where
                formatFile file = readFromFile (onInfo . processingFile) file >>= logErrorOr onInfo updateFile . (\i -> checkChange i <$> transform i)


data ValidateMode
    = ValidateStdin
    | ValidateFiles FilePath [FilePath]


validateNoChanges ::
    World m =>
    InfoFormatter.Loggable info =>
    ((FilePath, Text) -> Either info ())
    -> ValidateMode
    -> m Bool
validateNoChanges validate mode =
    let
        newValidate filePath content =
            case validate (filePath, content) of
                Left info -> Left (fmap InfoFormatter.aesonToText (InfoFormatter.jsonInfoMessage info))
                Right value -> Right value
    in
    case mode of
        ValidateStdin ->
            do
                (filePath, content) <- readStdin
                let result = newValidate filePath content
                World.putStrLn (InfoFormatter.resultsToJsonString [result])
                return (Either.isRight result)

        ValidateFiles first rest ->
            do
                results <- World.mapMConcurrently validateFile (first:rest)
                World.putStrLn (InfoFormatter.resultsToJsonString results)
                return (all Either.isRight results)
            where
                validateFile filePath =
                    do
                        content <- World.readUtf8File filePath
                        return (newValidate filePath content)


logErrorOr :: Monad m => (error -> m ()) -> (a -> m ()) -> Either error a -> m Bool
logErrorOr onInfo fn result =
    case result of
        Left message ->
            onInfo message *> return False

        Right value ->
            fn value *> return True

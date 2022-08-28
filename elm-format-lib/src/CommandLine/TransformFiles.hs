module CommandLine.TransformFiles
    ( Result
    , TransformMode(..), applyTransformation
    , ValidateMode(..), validateNoChanges
    ) where

-- This module provides reusable functions for command line tools that
-- transform files.

import CommandLine.InfoFormatter (ExecuteMode(..))
import qualified CommandLine.InfoFormatter as InfoFormatter
import CommandLine.World (World)
import qualified CommandLine.World as World
import Control.Monad.State hiding (runState)
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


readFromFile :: World m => (FilePath -> StateT s m ()) -> FilePath -> StateT s m (FilePath, Text)
readFromFile onProcessingFile filePath =
    onProcessingFile filePath
        *> lift (World.readUtf8FileWithPath filePath)


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

        infoMode = ForHuman usesStdout

        onInfo = InfoFormatter.onInfo infoMode

        approve = InfoFormatter.approve infoMode autoYes . confirmPrompt
    in
    runState (InfoFormatter.init infoMode) (InfoFormatter.done infoMode) $
    case mode of
        StdinToStdout ->
            lift (transform <$> readStdin) >>= logErrorOr onInfo (lift . World.writeStdout)

        StdinToFile outputFile ->
            lift (transform <$> readStdin) >>= logErrorOr onInfo (lift . World.writeUtf8File outputFile)

        FileToStdout inputFile ->
            lift (transform <$> World.readUtf8FileWithPath inputFile) >>= logErrorOr onInfo (lift . World.writeStdout)

        FileToFile inputFile outputFile ->
            (transform <$> readFromFile (onInfo . processingFile) inputFile) >>= logErrorOr onInfo (lift . World.writeUtf8File outputFile)

        FilesInPlace first rest ->
            do
                canOverwrite <- lift $ approve (first:rest)
                if canOverwrite
                    then all id <$> mapM formatFile (first:rest)
                    else return True
            where
                formatFile file = ((\i -> checkChange i <$> transform i) <$> readFromFile (onInfo . processingFile) file) >>= logErrorOr onInfo (lift . updateFile)


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


runState :: Monad m => (m (), state) -> (state -> m ()) -> StateT state m result -> m result
runState (initM, initialState) done run =
    do
        initM
        (result, finalState) <- runStateT run initialState
        done finalState
        return result

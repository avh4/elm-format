module CommandLine.TransformFiles
    ( Result
    , TransformMode(..), applyTransformation
    , ValidateMode(..), validateNoChanges
    ) where

-- This module provides reusable functions for command line tools that
-- transform files.

import CommandLine.InfoFormatter (ExecuteMode(..), InfoFormatterT)
import qualified CommandLine.InfoFormatter as InfoFormatter
import CommandLine.World (World)
import qualified CommandLine.World as World
import Control.Monad.State hiding (runState)
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


readFromFile ::
    (MonadTrans t, World m, Applicative (t m)) =>
    (FilePath -> t m ()) -> FilePath -> t m (FilePath, Text)
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
    InfoFormatter.runInfoFormatter infoMode $
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
    forall m info.
    World m =>
    InfoFormatter.Loggable info =>
    (FilePath -> info)
    -> ((FilePath, Text) -> Either info ())
    -> ValidateMode
    -> m Bool
validateNoChanges processingFile validate mode =
    let
        infoMode = ForMachine
        onInfo = InfoFormatter.onInfo infoMode
    in
    InfoFormatter.runInfoFormatter infoMode $
    case mode of
        ValidateStdin ->
            lift (validate <$> readStdin) >>= logError onInfo

        ValidateFiles first rest ->
            and <$> mapM validateFile (first:rest)
            where
                validateFile :: FilePath -> InfoFormatterT m Bool
                validateFile file =
                    (validate <$> readFromFile (onInfo . processingFile) file)
                        >>= logError onInfo


logErrorOr :: Monad m => (error -> m ()) -> (a -> m ()) -> Either error a -> m Bool
logErrorOr onInfo fn result =
    case result of
        Left message ->
            onInfo message *> return False

        Right value ->
            fn value *> return True

logError :: Monad m => (error -> m ()) -> Either error () -> m Bool
logError onInfo =
    logErrorOr onInfo return

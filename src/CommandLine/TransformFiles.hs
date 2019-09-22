module CommandLine.TransformFiles (Result(..), TransformMode(..), applyTransformation, checkChange, readFromFile, readStdin) where

-- This module provides reusable functions for command line tools that
-- transform files.

import Control.Monad.Free
import Data.Text (Text)
import ElmFormat.FileStore (FileStore)
import ElmFormat.FileWriter (FileWriter)
import ElmFormat.InputConsole (InputConsole)
import ElmFormat.OutputConsole (OutputConsole)
import Messages.Formatter.Format
import Messages.Types

import qualified ElmFormat.InputConsole as InputConsole
import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.FileWriter as FileWriter
import qualified ElmFormat.OutputConsole as OutputConsole


data Result a
    = NoChange FilePath a
    | Changed FilePath a


checkChange :: Eq a => (FilePath, a) -> a -> Result a
checkChange (inputFile, inputText) outputText =
    if inputText == outputText
        then NoChange inputFile outputText
        else Changed inputFile outputText


fromResult :: Result a -> a
fromResult result =
    case result of
        NoChange _ text -> text
        Changed _ text -> text


updateFile :: FileWriter f => Result Text -> Free f ()
updateFile result =
    case result of
        NoChange _ _ -> return ()
        Changed outputFile outputText -> FileWriter.overwriteFile outputFile outputText


readStdin :: InputConsole f => Free f (FilePath, Text)
readStdin =
    (,) "<STDIN>" <$> InputConsole.readStdin


readFromFile :: (FileStore f, InfoFormatter f) => FilePath -> Free f (FilePath, Text)
readFromFile filePath =
    onInfo (ProcessingFile filePath)
        *> ((,) filePath <$> FileStore.readFile filePath)


data TransformMode
    = StdinToStdout
    | StdinToFile FilePath
    | FileToStdout FilePath
    | FileToFile FilePath FilePath
    | FilesInPlace FilePath [FilePath]


applyTransformation :: (InputConsole f, OutputConsole f, InfoFormatter f, FileStore f, FileWriter f) => ((FilePath, Text) -> Either InfoMessage (Result Text)) -> TransformMode -> Free f Bool
applyTransformation transform mode =
    case mode of
        StdinToStdout ->
            (fmap fromResult <$> transform <$> readStdin) >>= logErrorOr OutputConsole.writeStdout

        StdinToFile outputFile ->
            (fmap fromResult <$> transform <$> readStdin) >>= logErrorOr (FileWriter.overwriteFile outputFile)

        -- TODO: this prints "Processing such-and-such-a-file.elm" which makes the stdout invalid
        -- FileToStdout inputFile ->
        --     (fmap fromResult <$> transform <$> ElmFormat.readFile inputFile) >>= logErrorOr OutputConsole.writeStdout

        FileToFile inputFile outputFile ->
            (fmap fromResult <$> transform <$> readFromFile inputFile) >>= logErrorOr (FileWriter.overwriteFile outputFile)

        FilesInPlace first rest ->
            do
                canOverwrite <- approve $ FilesWillBeOverwritten (first:rest)
                if canOverwrite
                    then all id <$> mapM formatFile (first:rest)
                    else return True
            where
                formatFile file = (transform <$> readFromFile file) >>= logErrorOr updateFile


logErrorOr :: InfoFormatter f => (a -> Free f ()) -> Either InfoMessage a -> Free f Bool
logErrorOr fn result =
    case result of
        Left message ->
            onInfo message *> return False

        Right value ->
            fn value *> return True

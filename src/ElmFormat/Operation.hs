module ElmFormat.Operation (Operation, OperationF(..)) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Free
import ElmFormat.FileStore
import ElmFormat.FileWriter
import ElmFormat.InputConsole
import ElmFormat.OutputConsole
import Messages.Formatter.Format


class (FileStore f, InfoFormatter f, OutputConsole f) => Operation f


data OperationF a
    = InFileStore (FileStoreF a)
    | InInfoFormatter (InfoFormatterF a)
    | InOutputConsole (OutputConsoleF a)
    | InInputConsole (InputConsoleF a)
    | InFileWriter (FileWriterF a)


instance Operation OperationF


instance FileStore OperationF where
    readFile path = InFileStore $ readFile path
    stat path = InFileStore $ stat path
    listDirectory path = InFileStore $ listDirectory path
    makeAbsolute path = InFileStore $ makeAbsolute path


instance InfoFormatter OperationF where
    onInfo msg = InInfoFormatter $ onInfo msg
    approve prompt = InInfoFormatter $ approve prompt


instance InputConsole OperationF where
    readStdin = InInputConsole readStdin


instance OutputConsole OperationF where
    writeStdout content = InOutputConsole $ writeStdout content


instance FileWriter OperationF where
    writeFile path content = InFileWriter $ writeFile path content
    overwriteFile path content = InFileWriter $ overwriteFile path content


instance Functor OperationF where
    fmap f (InFileStore op) = InFileStore (fmap f op)
    fmap f (InInfoFormatter op) = InInfoFormatter (fmap f op)
    fmap f (InOutputConsole op) = InOutputConsole (fmap f op)
    fmap f (InInputConsole op) = InInputConsole (fmap f op)
    fmap f (InFileWriter op) = InFileWriter (fmap f op)


instance Operation f => Operation (Free f)

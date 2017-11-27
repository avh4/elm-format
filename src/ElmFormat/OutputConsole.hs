module ElmFormat.OutputConsole (OutputConsole, OutputConsoleF(..), writeStdout, execute) where

import Control.Monad.Free
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Char8 as Char8


class Functor f => OutputConsole f where
    writeStdout :: Text -> f ()


data OutputConsoleF a
    = WriteStdout Text a


instance Functor OutputConsoleF where
    fmap f (WriteStdout content a) = WriteStdout content (f a)


instance OutputConsole OutputConsoleF where
    writeStdout content = WriteStdout content ()


instance OutputConsole f => OutputConsole (Free f) where
    writeStdout content = liftF (writeStdout content)


execute :: OutputConsoleF a -> IO a
execute operation =
    case operation of
        WriteStdout content next ->
            (Char8.putStr $ Text.encodeUtf8 content) *> return next

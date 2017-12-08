module ElmFormat.OutputConsole (OutputConsole, OutputConsoleF(..), writeStdout, execute) where

import Control.Monad.Free
import Data.Text (Text)
import ElmFormat.World (World)
import qualified Data.Text.Encoding as Text
import qualified ElmFormat.World as World


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


execute :: World m => OutputConsoleF a -> m a
execute operation =
    case operation of
        WriteStdout content next ->
            World.writeStdout content *> return next

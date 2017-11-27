module ElmFormat.InputConsole (InputConsole, InputConsoleF(..), readStdin, execute) where

import Control.Monad.Free
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as Lazy


class Functor f => InputConsole f where
    readStdin :: f Text


data InputConsoleF a
    = ReadStdin (Text -> a)


instance Functor InputConsoleF where
    fmap f (ReadStdin a) = ReadStdin (f . a)


instance InputConsole InputConsoleF where
    readStdin = ReadStdin id


instance InputConsole f => InputConsole (Free f) where
    readStdin = liftF readStdin


execute :: InputConsoleF a -> IO a
execute operation =
    case operation of
        ReadStdin next ->
            next <$> Text.decodeUtf8 <$> Lazy.toStrict <$> Lazy.getContents

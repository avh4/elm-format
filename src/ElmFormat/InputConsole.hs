module ElmFormat.InputConsole (InputConsole, InputConsoleF(..), readStdin, execute) where

import Control.Monad.Free
import Data.Text (Text)
import ElmFormat.World


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


execute :: World m => InputConsoleF a -> m a
execute operation =
    case operation of
        ReadStdin next ->
            next <$> getStdin

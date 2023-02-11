module CommandLine.InfoFormatter
    ( ToConsole(..), Loggable(..)
    , approve
    , resultsToJsonString, aesonToText, putStrLn') where

import Prelude hiding (init, putStrLn)

import CommandLine.World (World)
import qualified CommandLine.World as World
import Data.Text (Text, intercalate)
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Maybe as Maybe
import qualified Data.Aeson.Encoding as AesonEncoding


class ToConsole a where
    toConsole :: a -> Text


class ToConsole a => Loggable a where
    jsonInfoMessage :: a -> Maybe Aeson.Encoding


approve :: (World m, ToConsole prompt) => Bool -> prompt -> m Bool
approve autoYes prompt =
    if autoYes then
        return True
    else
        putStrLn' False (toConsole prompt) *> World.getYesOrNo


putStrLn' :: World m => Bool -> Text -> m ()
putStrLn' usingStdout =
    -- we log to stdout unless it is being used for file output (in that case, we log to stderr)
    case usingStdout of
        True -> World.putStrLnStderr
        False -> World.putStrLn


resultsToJsonString :: [Either (Maybe Text) ()] -> Text
resultsToJsonString results =
    if null lines then "[]" else "[" <> intercalate "\n," lines <> "\n]"
    where
        lines = Maybe.mapMaybe handleResult results
        handleResult = \case
            Left info -> info
            Right () -> Nothing

aesonToText :: AesonEncoding.Encoding' a -> Text
aesonToText = T.decodeUtf8 . B.concat . LB.toChunks . AesonEncoding.encodingToLazyByteString

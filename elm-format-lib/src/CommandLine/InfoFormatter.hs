module CommandLine.InfoFormatter
    ( ToConsole(..), Loggable(..)
    , onInfo, approve
    , ExecuteMode(..), init, done
    ) where

import Prelude hiding (init, putStrLn)

import CommandLine.World (World)
import qualified CommandLine.World as World
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Aeson.Encoding.Internal as AesonInternal


class ToConsole a where
    toConsole :: a -> Text


class ToConsole a => Loggable a where
    jsonInfoMessage :: a -> Maybe Aeson.Encoding


onInfo :: (World m, Loggable info) => ExecuteMode -> info -> StateT Bool m ()
onInfo mode info =
    case mode of
        ForMachine ->
            maybe (lift $ return ()) json $ jsonInfoMessage info

        ForHuman usingStdout ->
            lift $ putStrLn' usingStdout (toConsole info)


approve :: (World m, ToConsole prompt) => ExecuteMode -> Bool -> prompt -> m Bool
approve mode autoYes prompt =
    case autoYes of
        True -> return True

        False ->
            case mode of
                ForMachine -> return False

                ForHuman usingStdout ->
                    putStrLn' usingStdout (toConsole prompt) *> World.getYesOrNo


data ExecuteMode
    = ForMachine
    | ForHuman { _usingStdout :: Bool }


init :: World m => ExecuteMode -> (m (), Bool)
init ForMachine = (World.putStr "[", False)
init (ForHuman _) = (return (), undefined)


done :: World m => ExecuteMode -> Bool -> m ()
done ForMachine _ = World.putStrLn "]"
done (ForHuman _) _ = return ()


putStrLn' :: World m => Bool -> Text -> m ()
putStrLn' usingStdout =
    -- we log to stdout unless it is being used for file output (in that case, we log to stderr)
    case usingStdout of
        True -> World.putStrLnStderr
        False -> World.putStrLn


json :: World m => Aeson.Encoding -> StateT Bool m ()
json jsvalue =
    do
        printComma <- get
        when printComma (lift $ World.putStr ",")
        lift $ World.putStrLn $ T.decodeUtf8 $ B.concat $ LB.toChunks $ AesonInternal.encodingToLazyByteString jsvalue
        put True

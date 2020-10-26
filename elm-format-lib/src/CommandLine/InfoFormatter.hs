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
import qualified Data.Text as Text
import ElmVersion (ElmVersion)
import qualified Text.JSON as Json


class ToConsole a where
    toConsole :: a -> Text


class ToConsole a => Loggable a where
    jsonInfoMessage :: ElmVersion -> a -> Maybe Json.JSValue -- TODO: remove ElmVersion


onInfo :: (World m, Loggable info) => ExecuteMode -> info -> StateT Bool m ()
onInfo mode info =
    case mode of
        ForMachine elmVersion ->
            maybe (lift $ return ()) json $ jsonInfoMessage elmVersion info

        ForHuman usingStdout ->
            lift $ putStrLn' usingStdout (toConsole info)


approve :: (World m, ToConsole prompt) => ExecuteMode -> Bool -> prompt -> m Bool
approve mode autoYes prompt =
    case autoYes of
        True -> return True

        False ->
            case mode of
                ForMachine _ -> return False

                ForHuman usingStdout ->
                    putStrLn' usingStdout (toConsole prompt) *> World.getYesOrNo


data ExecuteMode
    = ForMachine ElmVersion
    | ForHuman { _usingStdout :: Bool }


init :: World m => ExecuteMode -> (m (), Bool)
init (ForMachine _) = (World.putStr "[", False)
init (ForHuman _) = (return (), undefined)


done :: World m => ExecuteMode -> Bool -> m ()
done (ForMachine _) _ = World.putStrLn "]"
done (ForHuman _) _ = return ()


putStrLn' :: World m => Bool -> Text -> m ()
putStrLn' usingStdout =
    -- we log to stdout unless it is being used for file output (in that case, we log to stderr)
    case usingStdout of
        True -> World.putStrLnStderr
        False -> World.putStrLn


json :: World m => Json.JSValue -> StateT Bool m ()
json jsvalue =
    do
        printComma <- get
        when printComma (lift $ World.putStr ",")
        lift $ World.putStrLn $ Text.pack $ Json.encode jsvalue
        put True

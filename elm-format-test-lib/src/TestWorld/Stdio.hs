module TestWorld.Stdio (State, empty, fullStdout,fullStderr,getStdin,putStr,putStrLn,putStrStderr,putStrLnStderr,queueStdin) where

import Prelude hiding (putStr, putStrLn)
import Data.Text (Text)


data State =
    State
        { _queuedStdin :: Text
        , _stdout :: [Text]
        , _stderr :: [Text]
        }


empty :: State
empty = State "" [] []


fullStdout :: State -> Text
fullStdout =
    mconcat . reverse . _stdout


fullStderr :: State -> Text
fullStderr =
    mconcat . reverse . _stderr


getStdin :: State -> (Text, State)
getStdin state =
    ( _queuedStdin state
    , state { _queuedStdin = "" }
    )


putStr :: Text -> State -> State
putStr text state =
    state { _stdout = text : _stdout state}


putStrLn :: Text -> State -> State
putStrLn line =
    putStr (line <> "\n")


putStrStderr :: Text -> State -> State
putStrStderr text state =
    state { _stderr = text : _stderr state }


putStrLnStderr :: Text -> State -> State
putStrLnStderr line =
    putStrStderr (line <> "\n")


queueStdin :: Text -> State -> State
queueStdin newStdin state =
    state { _queuedStdin = newStdin }

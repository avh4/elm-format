{-# LANGUAGE TemplateHaskell #-}

module TestWorld.Stdio (Stdio, empty, fullStdout,fullStderr,getStdin,putStr,putStrLn,putStrStderr,putStrLnStderr,queueStdin) where

import Prelude hiding (putStr, putStrLn)
import Data.Text (Text)
import Lens.Micro.Platform (makeLenses, (^.))
import qualified Data.Text as Text


data Stdio =
    Stdio
        { _queuedStdin :: Text
        , _stdout :: [Text]
        , _stderr :: [Text]
        }
makeLenses ''Stdio

instance Show Stdio where
    show stdio = Text.unpack $ mconcat
        [ "Queued stdin:\n"
        , stdio ^. queuedStdin
        , "\nStdout:\n"
        , Text.intercalate "\n" $ stdio ^. stdout
        , "\nStderr:\n"
        , Text.intercalate "\n" $ stdio ^. stderr
        ]


empty :: Stdio
empty = Stdio "" [] []


fullStdout :: Stdio -> Text
fullStdout =
    mconcat . reverse . _stdout


fullStderr :: Stdio -> Text
fullStderr =
    mconcat . reverse . _stderr


getStdin :: Stdio -> (Text, Stdio)
getStdin state =
    ( _queuedStdin state
    , state { _queuedStdin = "" }
    )


putStr :: Text -> Stdio -> Stdio
putStr text state =
    state { _stdout = text : _stdout state}


putStrLn :: Text -> Stdio -> Stdio
putStrLn line =
    putStr (line <> "\n")


putStrStderr :: Text -> Stdio -> Stdio
putStrStderr text state =
    state { _stderr = text : _stderr state }


putStrLnStderr :: Text -> Stdio -> Stdio
putStrLnStderr line =
    putStrStderr (line <> "\n")


queueStdin :: Text -> Stdio -> Stdio
queueStdin newStdin state =
    state { _queuedStdin = newStdin }

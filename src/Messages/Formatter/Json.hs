{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Messages.Formatter.Json (format) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

import Messages.Formatter.Format
import Messages.Types


format :: InfoFormatter
format = InfoFormatter
    { onInfo = (\m -> maybe (return ()) BC.putStrLn $ showInfo m) }


showInfo :: InfoMessage -> Maybe B.ByteString

showInfo (ProcessingFiles _) =
    Nothing

showInfo (FileWouldChange file) =
    Just $ Json.encode $ Json.object
        [ "path" .= file
        , "line" .= (1 :: Int)
        , "column" .= (1 :: Int)
        , "message" .= ("File would be changed" :: String)
        ]

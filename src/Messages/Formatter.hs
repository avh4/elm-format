{-# OPTIONS_GHC -Wall #-}
module Messages.Formatter where

import Messages.Formatter.Format

import qualified Messages.Formatter.Json as Json
import qualified Messages.Formatter.HumanReadable as HumanReadable
import qualified Data.Map as Map
import qualified Data.List as List

import ElmVersion (ElmVersion)


defaultFormatterName :: String
defaultFormatterName = "human-readable"


defaultFormatter :: InfoFormatter
defaultFormatter = HumanReadable.format


formatters :: Map.Map String (ElmVersion -> InfoFormatter)
formatters = Map.fromList [
    (defaultFormatterName, const defaultFormatter),
    ("json", Json.format)
    ]


orderedFormatterNames :: [String]
orderedFormatterNames =
    List.sort . Map.keys $ formatters


readFormatter :: String -> Either String (ElmVersion -> InfoFormatter)
readFormatter name =
    case Map.lookup name formatters of
        Nothing ->
            Left $ "No formatter known by the name of " ++ name
        Just f ->
            Right f

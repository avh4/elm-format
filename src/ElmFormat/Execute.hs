module ElmFormat.Execute (forHuman, forMachine) where

{-| This module provides executors that can take streams of Operations and
perform IO.
-}

import ElmFormat.Operation
import ElmVersion

import qualified ElmFormat.FileStore as FileStore
import qualified Messages.Formatter.HumanReadable as HumanReadable
import qualified Messages.Formatter.Json as Json


{-| Execute Operations in a fashion appropriate for interacting with humans. -}
forHuman :: OperationF a -> IO a
forHuman operation =
    case operation of
        InFileStore op -> FileStore.execute op
        InInfoFormatter op -> HumanReadable.format op
        DeprecatedIO io -> io


{-| Execute Operations in a fashion appropriate for use by automated scripts. -}
forMachine :: ElmVersion -> OperationF a -> IO a
forMachine elmVersion operation =
    case operation of
        InFileStore op -> FileStore.execute op
        InInfoFormatter op -> Json.format elmVersion op
        DeprecatedIO io -> io

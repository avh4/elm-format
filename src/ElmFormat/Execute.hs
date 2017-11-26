{-# LANGUAGE Rank2Types #-}
module ElmFormat.Execute (forHuman, forMachine, run) where

{-| This module provides executors that can take streams of Operations and
perform IO.
-}

import Prelude hiding (init)
import Elm.Utils ((|>))
import Control.Monad.State
import Control.Monad.Free
import ElmFormat.Operation
import ElmVersion

import qualified ElmFormat.FileStore as FileStore
import qualified Messages.Formatter.HumanReadable as HumanReadable
import qualified Messages.Formatter.Json as Json


data Program opF state = Program
    { init :: (IO (), state)
    , step :: forall a. opF a -> StateT state IO a
    , done :: state -> IO ()
    }


run :: Program opF state -> Free opF a -> IO a
run program operations =
    do
        let Program init step done = program
        let (initIO, initState) = init
        initIO
        (result, finalState) <-
            operations
                |> foldFree step
                |> flip runStateT initState
        done finalState
        return result


{-| Execute Operations in a fashion appropriate for interacting with humans. -}
forHuman :: Bool -> OperationF a -> IO a
forHuman autoYes operation =
    case operation of
        InFileStore op -> FileStore.execute op
        InInfoFormatter op -> HumanReadable.format autoYes op
        DeprecatedIO io -> io


{-| Execute Operations in a fashion appropriate for use by automated scripts. -}
forMachine :: ElmVersion -> Bool -> Program OperationF Bool
forMachine elmVersion autoYes =
    Program
        { init = Json.init
        , step = \operation ->
            case operation of
                InFileStore op -> lift $ FileStore.execute op
                InInfoFormatter op -> Json.format elmVersion autoYes op
                DeprecatedIO io -> lift io
        , done = const Json.done
        }

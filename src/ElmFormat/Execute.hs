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
import ElmFormat.World

import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.FileWriter as FileWriter
import qualified ElmFormat.InputConsole as InputConsole
import qualified ElmFormat.OutputConsole as OutputConsole
import qualified Messages.Formatter.HumanReadable as HumanReadable
import qualified Messages.Formatter.Json as Json


data Program m opF state = Program
    { init :: (m (), state)
    , step :: forall a. opF a -> StateT state m a
    , done :: state -> m ()
    }


run :: World m => Program m opF state -> Free opF a -> m a
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
forHuman :: World m => Bool -> Program m OperationF ()
forHuman autoYes =
    Program
        { init = (return (), ())
        , step = \operation ->
              case operation of
                  InFileStore op -> lift $ FileStore.execute op
                  InInfoFormatter op -> lift $ HumanReadable.format autoYes op
                  InInputConsole op -> lift $ InputConsole.execute op
                  InOutputConsole op -> lift $ OutputConsole.execute op
                  InFileWriter op -> lift $ FileWriter.execute op
        , done = \() -> return ()
        }


{-| Execute Operations in a fashion appropriate for use by automated scripts. -}
forMachine :: World m => Bool -> Program m OperationF Bool
forMachine autoYes =
    Program
        { init = Json.init
        , step = \operation ->
            case operation of
                InFileStore op -> lift $ FileStore.execute op
                InInfoFormatter op -> Json.format autoYes op
                InInputConsole op -> lift $ InputConsole.execute op
                InOutputConsole op -> lift $ OutputConsole.execute op
                InFileWriter op -> lift $ FileWriter.execute op
        , done = const Json.done
        }

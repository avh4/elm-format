{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Elm.Utils
    ( (|>), (<|), (>>)
    , run, unwrappedRun
    , CommandError(..)
    ) where

import Prelude hiding ((>>))
import Control.Monad.Except (MonadError, MonadIO, liftIO, throwError)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)


{-| Forward function application `x |> f == f x`. This function is useful
for avoiding parenthesis and writing code in a more natural way.
-}
(|>) :: a -> (a -> b) -> b
x |> f = f x


{-| Backward function application `f <| x == f x`. This function is useful for
avoiding parenthesis.
-}
(<|) :: (a -> b) -> a -> b
f <| x = f x


infixr 1 <|
infixl 1 |>


(>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >> g = \x -> f x |> g


-- RUN EXECUTABLES

data CommandError
    = MissingExe String
    | CommandFailed String String


{-| Run a command, throw an error if the command is not found or if something
goes wrong.
-}
run :: (MonadError String m, MonadIO m) => String -> [String] -> m String
run command args =
  do  result <- liftIO (unwrappedRun command args)
      case result of
        Right out -> return out
        Left err ->
          throwError (context (message err))
  where
    context msg =
      "failure when running:" ++ concatMap (' ':) (command:args) ++ "\n" ++ msg

    message err =
      case err of
        CommandFailed stderr stdout ->
          stdout ++ stderr
        MissingExe msg ->
          msg


unwrappedRun :: String -> [String] -> IO (Either CommandError String)
unwrappedRun command args =
  do  (exitCode, stdout, stderr) <- readProcessWithExitCode command args ""
      return $
          case exitCode of
            ExitSuccess -> Right stdout
            ExitFailure code
                | code == 127  -> Left (missingExe command)  -- UNIX
                | code == 9009 -> Left (missingExe command)  -- Windows
                | otherwise    -> Left (CommandFailed stdout stderr)


missingExe :: String -> CommandError
missingExe command =
  MissingExe $
    "Could not find command `" ++ command ++ "`. Do you have it installed?\n\
    \    Can it be run from anywhere? Is it on your PATH?"

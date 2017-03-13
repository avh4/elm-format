{-# OPTIONS_GHC -Wall #-}
module Messages.Formatter.HumanReadable (format) where

import Messages.Formatter.Format
import Messages.Types
import CommandLine.Helpers (showErrors)


format :: InfoFormatterF a -> IO a
format infoFormatter =
    case infoFormatter of
        OnInfo info next ->
            renderInfo info
                *> return next


renderInfo :: InfoMessage -> IO ()

renderInfo (ProcessingFiles files) =
    case files of
        [file] ->
            putStrLn $ "Processing file " ++ file
        _ ->
            putStrLn "Processing multiple files..."

renderInfo (FileWouldChange file) =
    putStrLn $ "File would be changed " ++ file

renderInfo (ParseError inputFile inputText errs) =
    showErrors inputFile inputText errs

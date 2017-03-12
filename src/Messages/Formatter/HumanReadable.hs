{-# OPTIONS_GHC -Wall #-}
module Messages.Formatter.HumanReadable (format) where

import Messages.Formatter.Format
import Messages.Types


format :: InfoFormatter
format = InfoFormatter
    { onInfo = renderInfo }


renderInfo :: InfoMessage -> IO ()

renderInfo (ProcessingFiles files) =
    case files of
        [file] ->
            putStrLn $ "Processing file " ++ file
        _ ->
            putStrLn "Processing multiple files..."

renderInfo (FileWouldChange file) =
    putStrLn $ "File would be changed " ++ file

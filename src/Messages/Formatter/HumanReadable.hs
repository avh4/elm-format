{-# OPTIONS_GHC -Wall #-}
module Messages.Formatter.HumanReadable (format) where

import Prelude hiding (getLine, putStr, putStrLn)

import Messages.Formatter.Format
import Messages.Types
import CommandLine.Helpers (showErrors)
import Messages.Strings (showPromptMessage)
import System.IO (hFlush, stdout)
import ElmFormat.World


format :: World m => Bool -> InfoFormatterF a -> m a
format autoYes infoFormatter =
    case infoFormatter of
        OnInfo info next ->
            renderInfo info
                *> return next

        Approve prompt next ->
            case autoYes of
                True -> return (next True)
                False ->
                    putStrLn (showPromptMessage prompt)
                        *> fmap next yesOrNo


yesOrNo :: World m => m Bool
yesOrNo =
  do  flushStdout
      input <- getLine
      case input of
        "y" -> return True
        "n" -> return False
        _   -> do putStr "Must type 'y' for yes or 'n' for no: "
                  yesOrNo


renderInfo :: World m => InfoMessage -> m ()

renderInfo (ProcessingFile file) =
    putStrLn $ "Processing file " ++ file

renderInfo (FileWouldChange file) =
    putStrLn $ "File would be changed " ++ file

renderInfo (ParseError inputFile inputText errs) =
    showErrors inputFile inputText errs

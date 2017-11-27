{-# OPTIONS_GHC -Wall #-}
module Messages.Formatter.HumanReadable (format) where

import Messages.Formatter.Format
import Messages.Types
import CommandLine.Helpers (showErrors)
import Messages.Strings (showPromptMessage)
import System.IO (hFlush, stdout)


format :: Bool -> InfoFormatterF a -> IO a
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


yesOrNo :: IO Bool
yesOrNo =
  do  hFlush stdout
      input <- getLine
      case input of
        "y" -> return True
        "n" -> return False
        _   -> do putStr "Must type 'y' for yes or 'n' for no: "
                  yesOrNo


renderInfo :: InfoMessage -> IO ()

renderInfo (ProcessingFile file) =
    putStrLn $ "Processing file " ++ file

renderInfo (FileWouldChange file) =
    putStrLn $ "File would be changed " ++ file

renderInfo (ParseError inputFile inputText errs) =
    showErrors inputFile inputText errs

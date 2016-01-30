module CommandLine.Helpers where

import System.IO
import System.Exit (exitFailure, exitSuccess)
import Messages.Types (Message(..))
import Messages.Strings (renderMessage)

import qualified Reporting.Annotation as RA
import qualified Reporting.Report as Report
import qualified Reporting.Error.Syntax as Syntax


r :: Message -> String
r = renderMessage

yesOrNo :: IO Bool
yesOrNo =
  do  hFlush stdout
      input <- getLine
      case input of
        "y" -> return True
        "n" -> return False
        _   -> do putStr "Must type 'y' for yes or 'n' for no: "
                  yesOrNo


decideOutputFile :: Bool -> FilePath -> Maybe FilePath -> IO FilePath
decideOutputFile autoYes inputFile outputFile =
    case outputFile of
        Nothing -> do -- we are overwriting the input file
            canOverwrite <- getApproval autoYes [inputFile]
            case canOverwrite of
                True -> return inputFile
                False -> exitSuccess
        Just outputFile' -> return outputFile'


getApproval :: Bool -> [FilePath] -> IO Bool
getApproval autoYes filePaths =
    case autoYes of
        True ->
            return True
        False -> do
            putStrLn $ (r $ FilesWillBeOverwritten filePaths)
            yesOrNo


exitFilesNotFound :: [FilePath] -> IO ()
exitFilesNotFound filePaths = do
    putStrLn $ (r $ NoElmFilesFound filePaths)
    exitFailure


exitTooManyInputSources :: IO ()
exitTooManyInputSources = do
    putStrLn $ (r $ TooManyInputSources)
    exitFailure


exitOnInputDirAndOutput :: IO ()
exitOnInputDirAndOutput = do
    putStrLn $ r CantWriteToOutputBecauseInputIsDirectory
    exitFailure


showErrors :: String -> [RA.Located Syntax.Error] ->  IO ()
showErrors source errs = do
    putStrLn (r ErrorsHeading)
    mapM_ (printError source) errs


printError :: String -> RA.Located Syntax.Error -> IO ()
printError source (RA.A range err) =
    Report.printError (r ErrorFileLocation) range (Syntax.toReport err) source

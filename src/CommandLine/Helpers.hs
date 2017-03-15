module CommandLine.Helpers where

import Control.Monad.Free
import ElmFormat.Operation (Operation)
import System.IO
import System.Exit (exitFailure, exitSuccess)
import Messages.Types (ErrorMessage(..), PromptMessage(..))
import Messages.Strings (showErrorMessage, showPromptMessage)

import qualified ElmFormat.Operation as Operation
import qualified Reporting.Annotation as RA
import qualified Reporting.Report as Report
import qualified Reporting.Error.Syntax as Syntax


r :: ErrorMessage -> String
r = showErrorMessage

yesOrNo :: IO Bool
yesOrNo =
  do  hFlush stdout
      input <- getLine
      case input of
        "y" -> return True
        "n" -> return False
        _   -> do putStr "Must type 'y' for yes or 'n' for no: "
                  yesOrNo


decideOutputFile :: Operation f => Bool -> FilePath -> Maybe FilePath -> Free f FilePath
decideOutputFile autoYes inputFile outputFile =
    case outputFile of
        Nothing -> do -- we are overwriting the input file
            canOverwrite <- getApproval autoYes [inputFile]
            case canOverwrite of
                True -> return inputFile
                False -> Operation.deprecatedIO exitSuccess
        Just outputFile' -> return outputFile'


getApproval :: Operation f => Bool -> [FilePath] -> Free f Bool
getApproval autoYes filePaths =
    case autoYes of
        True ->
            return True
        False -> Operation.deprecatedIO $ do
            putStrLn $ (showPromptMessage $ FilesWillBeOverwritten filePaths)
            yesOrNo


exitOnInputDirAndOutput :: Operation f => f ()
exitOnInputDirAndOutput = Operation.deprecatedIO $ do
    putStrLn $ r SingleOutputWithMultipleInputs
    exitFailure


showErrors :: String -> String -> [RA.Located Syntax.Error] ->  IO ()
showErrors filename source errs = do
    putStrLn (r ErrorsHeading)
    mapM_ (printError filename source) errs


printError :: String -> String -> RA.Located Syntax.Error -> IO ()
printError filename source (RA.A range err) =
    Report.printError filename range (Syntax.toReport err) source

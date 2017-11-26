module CommandLine.Helpers where

import Control.Monad.Free
import ElmFormat.Operation (Operation)
import Messages.Formatter.Format (InfoFormatter, approve)
import System.IO
import System.Exit (exitFailure)
import Messages.Types (ErrorMessage(..), PromptMessage(..))
import Messages.Strings (showErrorMessage)

import qualified ElmFormat.Operation as Operation
import qualified Reporting.Annotation as RA
import qualified Reporting.Report as Report
import qualified Reporting.Error.Syntax as Syntax


r :: ErrorMessage -> String
r = showErrorMessage


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

module CommandLine.Helpers where

import Messages.Types (ErrorMessage(..))
import Messages.Strings (showErrorMessage)

import qualified Reporting.Annotation as RA
import qualified Reporting.Report as Report
import qualified Reporting.Error.Syntax as Syntax


r :: ErrorMessage -> String
r = showErrorMessage


showErrors :: String -> String -> [RA.Located Syntax.Error] ->  IO ()
showErrors filename source errs = do
    putStrLn (r ErrorsHeading)
    mapM_ (printError filename source) errs


printError :: String -> String -> RA.Located Syntax.Error -> IO ()
printError filename source (RA.A range err) =
    Report.printError filename range (Syntax.toReport err) source

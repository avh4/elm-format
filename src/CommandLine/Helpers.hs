module CommandLine.Helpers where

import Prelude hiding (putStrLn)

import Messages.Types (ErrorMessage(..))
import Messages.Strings (showErrorMessage)
import ElmFormat.World

import qualified Reporting.Annotation as RA
import qualified Reporting.Report as Report
import qualified Reporting.Error.Syntax as Syntax


r :: ErrorMessage -> String
r = showErrorMessage


showErrors :: World m => String -> String -> [RA.Located Syntax.Error] ->  m ()
showErrors filename source errs = do
    putStrLnStderr (r ErrorsHeading)
    mapM_ (printError filename source) errs


printError :: World m => String -> String -> RA.Located Syntax.Error -> m ()
printError filename source (RA.A range err) =
    Report.printError filename range (Syntax.toReport err) source

module CommandLine.Helpers where

import Prelude hiding (putStrLn)

import Messages.Types (ErrorMessage(..))
import Messages.Strings (showErrorMessage)
import ElmFormat.World

import qualified Reporting.Annotation as RA
import qualified Reporting.Report as Report
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Doc as Doc


r :: ErrorMessage -> String
r = showErrorMessage


showErrors :: World m => String -> String -> [RA.Located Syntax.Error] ->  m ()
showErrors filename source errs = do
    putStrLn (r ErrorsHeading)
    mapM_ (printError filename source) errs


printError :: World m => String -> String -> RA.Located Syntax.Error -> m ()
printError filename source (RA.A range err) =
    Doc.toAnsi $ Report.toDoc filename range (Syntax.toReport err) source

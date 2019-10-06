module Messages.Types where

import CommandLine.ResolveFiles (ResolveFileError)
import ElmVersion
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Syntax

-- inspired by:
-- https://wiki.haskell.org/Internationalization_of_Haskell_programs_using_Haskell_data_types

data InfoMessage
  = ProcessingFile FilePath
  | FileWouldChange FilePath
  | ParseError FilePath String [A.Located Syntax.Error]


data PromptMessage
    = FilesWillBeOverwritten [FilePath]


data ErrorMessage
  = ErrorsHeading

  | BadInputFiles [ResolveFileError]
  | NoInputs
  | SingleOutputWithMultipleInputs
  | TooManyInputs
  | OutputAndValidate
  | MustSpecifyVersionWithUpgrade ElmVersion

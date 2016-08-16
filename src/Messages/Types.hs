{-# OPTIONS_GHC -Wall #-}
module Messages.Types where

-- inspired by:
-- https://wiki.haskell.org/Internationalization_of_Haskell_programs_using_Haskell_data_types

data InfoMessage
  = ProcessingFiles [FilePath]
  | FileWouldChange FilePath


data ErrorMessage
  = ErrorsHeading

  | FilesWillBeOverwritten [FilePath]
  | BadInputFiles [InputFileMessage]
  | NoInputs
  | SingleOutputWithMultipleInputs
  | TooManyInputs
  | OutputAndValidate


data InputFileMessage
    = FileDoesNotExist FilePath
    | NoElmFiles FilePath

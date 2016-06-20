{-# OPTIONS_GHC -Wall #-}
module Messages.Types where

-- inspired by:
-- https://wiki.haskell.org/Internationalization_of_Haskell_programs_using_Haskell_data_types

data Message
  = ErrorsHeading

  | FilesWillBeOverwritten [FilePath]
  | BadInputFiles [InputFileMessage]
  | Error_NoInputs
  | Error_SingleOutputWithMultipleInputs
  | Error_TooManyInputs
  | Error_OutputAndValidate
  | ProcessingFiles [FilePath]
  | FileWouldChange FilePath


data InputFileMessage
    = FileDoesNotExist FilePath
    | NoElmFiles FilePath

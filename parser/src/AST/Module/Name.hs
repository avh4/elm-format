module AST.Module.Name where

import qualified Data.List as List


type Raw = [String] -- must be non-empty


toString :: Raw -> String
toString rawName =
  List.intercalate "." rawName

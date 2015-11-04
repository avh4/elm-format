module AST.Module.Name where

import qualified Data.List as List
import qualified Elm.Package as Package


type Raw = [String] -- must be non-empty


data Canonical = Canonical
    { _package :: Package.Name
    , _module :: Raw
    }
    deriving (Eq, Ord, Show)


toString :: Raw -> String
toString rawName =
  List.intercalate "." rawName

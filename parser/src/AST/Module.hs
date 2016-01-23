module AST.Module
    ( Module(..), Header(..)
    , UserImport(..), ImportMethod(..)
    , stripRegion
    ) where

import qualified AST.Declaration as Declaration
import qualified AST.Module.Name as Name
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import AST.V0_16


-- MODULES


data Module = Module
    { header :: Header
    , body :: [Declaration.Decl]
    }
    deriving (Eq, Show)


stripRegion m =
    Module
    { header =
        Header
          { name = name $ header m
          , docs = A.stripRegion $ docs $ header m
          , exports = exports $ header m
          , postExportComments = postExportComments $ header m
          , imports = imports $ header m
          }
    , body = map Declaration.stripRegion' $ body m
    }


-- HEADERS

{-| Basic info needed to identify modules and determine dependencies. -}
data Header = Header
    { name :: Commented Name.Raw
    , docs :: A.Located (Maybe String)
    , exports :: Var.Listing Var.Value
    , postExportComments :: Comments
    , imports :: [UserImport]
    }
    deriving (Eq, Show)


-- IMPORTs

data UserImport
    = UserImport (A.Located (Name.Raw, ImportMethod))
    | ImportComment Comment
    deriving (Eq, Show)


data ImportMethod = ImportMethod
    { alias :: Maybe String
    , exposedVars :: !(Var.Listing Var.Value)
    }
    deriving (Eq, Show)

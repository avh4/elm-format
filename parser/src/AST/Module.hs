module AST.Module
    ( Module(..), Body(..)
    , Header(..)
    , UserImport, DefaultImport, ImportMethod(..)
    , stripRegion
    ) where

import qualified AST.Declaration as Declaration
import qualified AST.Module.Name as Name
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


-- MODULES


data Module = Module
    { name    :: Name.Raw
    , docs    :: A.Located (Maybe String)
    , exports :: Var.Listing (A.Located Var.Value)
    , imports :: [UserImport]
    , body    :: [Declaration.Decl]
    }
    deriving (Eq, Show)


stripRegion m =
    Module
    { name = name m
    , docs = A.stripRegion $ docs m
    , exports = fmap A.stripRegion $ exports m
    , imports = imports m
    , body = map Declaration.stripRegion' $ body m
    }

data Body expr = Body
    { program   :: expr
    , fixities  :: [(Declaration.Assoc, Int, String)]
    , ports     :: [String]
    }


-- HEADERS

{-| Basic info needed to identify modules and determine dependencies. -}
data Header = Header
    { _name :: Name.Raw
    , _docs :: A.Located (Maybe String)
    , _exports :: Var.Listing (A.Located Var.Value)
    , _imports :: [UserImport]
    }


-- IMPORTs

type UserImport = A.Located (Name.Raw, ImportMethod)


type DefaultImport = (Name.Raw, ImportMethod)


data ImportMethod = ImportMethod
    { alias :: Maybe String
    , exposedVars :: !(Var.Listing Var.Value)
    }
    deriving (Eq, Show)

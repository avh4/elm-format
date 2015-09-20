module AST.Module
    ( Interfaces
    , Types, Aliases, ADTs
    , AdtInfo, CanonicalAdt
    , Module(..), Body(..)
    , Header(..)
    , UserImport, DefaultImport, ImportMethod(..)
    ) where

import Control.Applicative ((<$>),(<*>))
import Data.Binary
import qualified Data.Map as Map

import qualified AST.Declaration as Decl
import qualified AST.Module.Name as Name
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Docs.AST as Docs
import qualified Elm.Package as Package
import qualified Reporting.Annotation as A


-- HELPFUL TYPE ALIASES

type Interfaces = Map.Map Name.Canonical Interface

type Types   = Map.Map String Type.Canonical
type Aliases = Map.Map String ([String], Type.Canonical)
type ADTs    = Map.Map String (AdtInfo String)

type AdtInfo v = ( [String], [(v, [Type.Canonical])] )
type CanonicalAdt = (Var.Canonical, AdtInfo Var.Canonical)


-- MODULES


data Module = Module
    { name    :: Name.Canonical
    , path    :: FilePath
    , docs    :: A.Located (Maybe String)
    , exports :: (Var.Listing (A.Located Var.Value))
    , imports :: [UserImport]
    , body    :: [Decl.SourceDecl]
    }


data Body expr = Body
    { program   :: expr
    , types     :: Types
    , fixities  :: [(Decl.Assoc, Int, String)]
    , aliases   :: Aliases
    , datatypes :: ADTs
    , ports     :: [String]
    }


-- HEADERS

{-| Basic info needed to identify modules and determine dependencies. -}
data Header imports = Header
    { _name :: Name.Raw
    , _docs :: A.Located (Maybe String)
    , _exports :: Var.Listing (A.Located Var.Value)
    , _imports :: imports
    }


-- IMPORTs

type UserImport = A.Located (Name.Raw, ImportMethod)


type DefaultImport = (Name.Raw, ImportMethod)


data ImportMethod = ImportMethod
    { alias :: Maybe String
    , exposedVars :: !(Var.Listing Var.Value)
    }


-- INTERFACES

{-| Key facts about a module, used when reading info from .elmi files. -}
data Interface = Interface
    { iVersion  :: Package.Version
    , iPackage  :: Package.Name
    , iExports  :: [Var.Value]
    , iTypes    :: Types
    , iImports  :: [Name.Raw]
    , iAdts     :: ADTs
    , iAliases  :: Aliases
    , iFixities :: [(Decl.Assoc, Int, String)]
    , iPorts    :: [String]
    }

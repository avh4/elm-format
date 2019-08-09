module AST.Module
    ( Module(..), Header(..), SourceTag(..), SourceSettings
    , UserImport, ImportMethod(..)
    , DetailedListing(..)
    , defaultHeader
    ) where

import AST.Declaration (TopLevelStructure, Declaration)
import qualified AST.Variable as Var
import qualified Cheapskate.Types as Markdown
import Data.Map.Strict (Map)
import qualified Reporting.Annotation as A
import AST.V0_16


-- MODULES


data Module = Module
    { initialComments :: Comments
    , header :: Maybe Header
    , docs :: A.Located (Maybe Markdown.Blocks)
    , imports :: PreCommented (Map [UppercaseIdentifier] (Comments, ImportMethod))
    , body :: [TopLevelStructure Declaration]
    }
    deriving (Eq, Show)


instance A.Strippable Module where
  stripRegion m =
    Module
    { initialComments = initialComments m
    , header = header m
    , docs = A.stripRegion $ docs m
    , imports = imports m
    , body = map A.stripRegion $ body m
    }


-- HEADERS

data SourceTag
  = Normal
  | Effect Comments
  | Port Comments
  deriving (Eq, Show)


{-| Basic info needed to identify modules and determine dependencies. -}
data Header = Header
    { srcTag :: SourceTag
    , name :: Commented [UppercaseIdentifier]
    , moduleSettings :: Maybe (KeywordCommented SourceSettings)
    , exports :: Maybe (KeywordCommented (Var.Listing DetailedListing))
    }
    deriving (Eq, Show)


defaultHeader :: Header
defaultHeader =
    Header
        Normal
        (Commented [] [UppercaseIdentifier "Main"] [])
        Nothing
        Nothing


data DetailedListing = DetailedListing
    { values :: Var.CommentedMap LowercaseIdentifier ()
    , operators :: Var.CommentedMap SymbolIdentifier ()
    , types :: Var.CommentedMap UppercaseIdentifier (Comments, Var.Listing (Var.CommentedMap UppercaseIdentifier ()))
    }
    deriving (Eq, Show)


type SourceSettings =
  [(Commented LowercaseIdentifier, Commented UppercaseIdentifier)]

-- IMPORTs

type UserImport
    = (PreCommented [UppercaseIdentifier], ImportMethod)


data ImportMethod = ImportMethod
    { alias :: Maybe (Comments, PreCommented UppercaseIdentifier)
    , exposedVars :: (Comments, PreCommented (Var.Listing DetailedListing))
    }
    deriving (Eq, Show)

module AST.Module
    ( Module(..), Header(..), SourceTag(..)
    , UserImport(..), ImportMethod(..)
    ) where

import qualified AST.Declaration as Declaration
import qualified AST.Variable as Var
import qualified Cheapskate.Types as Markdown
import qualified Reporting.Annotation as A
import AST.V0_16


-- MODULES


data Module = Module
    { initialComments :: Comments
    , header :: Header
    , docs :: A.Located (Maybe Markdown.Blocks)
    , imports :: [UserImport]
    , body :: [Declaration.Decl]
    }
    deriving (Eq, Show)


instance A.Strippable Module where
  stripRegion m =
    Module
    { initialComments = initialComments m
    , header =
        Header
          { srcTag = srcTag $ header m
          , name = name $ header m
          , moduleSettings = moduleSettings $ header m
          , exports = exports $ header m
          }
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
    , exports :: KeywordCommented (Var.Listing Var.Value)
    }
    deriving (Eq, Show)


type SourceSettings =
  [(Commented LowercaseIdentifier, Commented UppercaseIdentifier)]

-- IMPORTs

data UserImport
    = UserImport (A.Located (PreCommented [UppercaseIdentifier], ImportMethod))
    | ImportComment Comment
    deriving (Eq, Show)


data ImportMethod = ImportMethod
    { alias :: Maybe (Comments, PreCommented UppercaseIdentifier)
    , exposedVars :: (Comments, PreCommented (Var.Listing Var.Value))
    }
    deriving (Eq, Show)

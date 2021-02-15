{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}

module AST.Module
    ( Module(..), Header(..), SourceTag(..), SourceSettings
    , UserImport, ImportMethod(..)
    , DetailedListing(..)
    , defaultHeader
    ) where

import AST.Listing (Listing)
import qualified AST.Listing as Listing
import qualified Cheapskate.Types as Markdown
import Data.Map.Strict (Map)
import qualified Reporting.Annotation as A
import AST.V0_16


-- MODULES


data Module ns body =
    Module
    { initialComments :: Comments
    , header :: Maybe Header
    , docs :: A.Located (Maybe Markdown.Blocks)
    , imports :: C1 'BeforeTerm (Map ns (C1 'BeforeTerm ImportMethod))
    , body :: body
    }
    deriving (Eq, Show, Functor)


-- HEADERS

data SourceTag
  = Normal
  | Effect Comments
  | Port Comments
  deriving (Eq, Show)


{-| Basic info needed to identify modules and determine dependencies. -}
data Header = Header
    { srcTag :: SourceTag
    , name :: C2 'BeforeTerm 'AfterTerm [UppercaseIdentifier]
    , moduleSettings :: Maybe (C2 'BeforeSeparator 'AfterSeparator SourceSettings)
    , exports :: Maybe (C2 'BeforeSeparator 'AfterSeparator (Listing DetailedListing))
    }
    deriving (Eq, Show)


defaultHeader :: Header
defaultHeader =
    Header
        Normal
        (C ([], []) [UppercaseIdentifier "Main"])
        Nothing
        Nothing


data DetailedListing = DetailedListing
    { values :: Listing.CommentedMap LowercaseIdentifier ()
    , operators :: Listing.CommentedMap SymbolIdentifier ()
    , types :: Listing.CommentedMap UppercaseIdentifier (C1 'BeforeTerm (Listing (Listing.CommentedMap UppercaseIdentifier ())))
    }
    deriving (Eq, Show)

instance Semigroup DetailedListing where
    (DetailedListing av ao at) <> (DetailedListing bv bo bt) = DetailedListing (av <> bv) (ao <> bo) (at <> bt)

instance Monoid DetailedListing where
    mempty = DetailedListing mempty mempty mempty


type SourceSettings =
    [ ( C2 'BeforeTerm 'AfterTerm LowercaseIdentifier
      , C2 'BeforeTerm 'AfterTerm UppercaseIdentifier
      )
    ]

-- IMPORTs

type UserImport
    = (C1 'BeforeTerm [UppercaseIdentifier], ImportMethod)


data ImportMethod = ImportMethod
    { alias :: Maybe (C2 'BeforeSeparator 'AfterSeparator UppercaseIdentifier)
    , exposedVars :: C2 'BeforeSeparator 'AfterSeparator (Listing DetailedListing)
    }
    deriving (Eq, Show)

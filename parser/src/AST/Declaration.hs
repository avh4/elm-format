{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module AST.Declaration where

import AST.V0_16
import ElmFormat.Mapping

import AST.Pattern (Pattern)
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Cheapskate.Types as Markdown


-- DECLARATIONS

type NameWithArgs name arg =
    (name, [PreCommented arg])

data Declaration ns expr
    = Definition (Pattern ns) [PreCommented (Pattern ns)] Comments expr
    | TypeAnnotation (PostCommented (Var.Ref ())) (PreCommented (Type ns))
    | Datatype
        { nameWithArgs :: Commented (NameWithArgs UppercaseIdentifier LowercaseIdentifier)
        , tags :: OpenCommentedList (NameWithArgs UppercaseIdentifier (Type ns))
        }
    | TypeAlias Comments
        (Commented (NameWithArgs UppercaseIdentifier LowercaseIdentifier))
        (PreCommented (Type ns))
    | PortAnnotation (Commented LowercaseIdentifier) Comments (Type ns)
    | PortDefinition (Commented LowercaseIdentifier) Comments expr
    | Fixity Assoc Comments Int Comments (Var.Ref ns)
    | Fixity_0_19 (PreCommented Assoc) (PreCommented Int) (Commented SymbolIdentifier) (PreCommented LowercaseIdentifier)
    deriving (Eq, Show, Functor)


instance MapNamespace a b (Declaration a e) (Declaration b e) where
    mapNamespace f = \case
        Definition first rest comments e -> Definition (fmap (fmap f) first) (fmap (fmap $ fmap $ fmap f) rest) comments e
        TypeAnnotation name typ -> TypeAnnotation name (fmap (fmap $ fmap f) typ)
        Datatype nameWithArgs tags -> Datatype nameWithArgs (fmap (fmap $ fmap $ fmap $ fmap $ fmap f) tags)
        TypeAlias comments name typ -> TypeAlias comments name (fmap (fmap $ fmap f) typ)
        PortAnnotation name comments typ -> PortAnnotation name comments (fmap (fmap f) typ)
        PortDefinition name comments expr -> PortDefinition name comments expr
        Fixity a pre n post op -> Fixity a pre n post (fmap f op)
        Fixity_0_19 a n op fn -> Fixity_0_19 a n op fn


instance MapReferences a b (Declaration a e) (Declaration b e) where
    mapReferences fu fl = \case
        Definition first rest comments e -> Definition (mapReferences fu fl first) (mapReferences fu fl rest) comments e
        TypeAnnotation name typ -> TypeAnnotation name (mapReferences fu fl typ)
        Datatype nameWithArgs tags -> Datatype nameWithArgs (fmap (fmap $ fmap $ fmap $ mapReferences fu fl) tags)
        TypeAlias comments name typ -> TypeAlias comments name (mapReferences fu fl typ)
        PortAnnotation name comments typ -> PortAnnotation name comments (mapReferences fu fl typ)
        PortDefinition name comments expr -> PortDefinition name comments expr
        Fixity a pre n post op -> Fixity a pre n post (mapReferences fu fl op)
        Fixity_0_19 a n op fn -> Fixity_0_19 a n op fn


-- INFIX STUFF

data Assoc = L | N | R
    deriving (Eq, Show)


assocToString :: Assoc -> String
assocToString assoc =
    case assoc of
      L -> "left"
      N -> "non"
      R -> "right"


-- DECLARATION PHASES


data TopLevelStructure a
    = DocComment Markdown.Blocks
    | BodyComment Comment
    | Entry (A.Located a)
    deriving (Eq, Show, Functor)

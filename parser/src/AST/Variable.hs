module AST.Variable where

import AST.V0_16


data Ref
    = VarRef [UppercaseIdentifier] LowercaseIdentifier
    | TagRef [UppercaseIdentifier] UppercaseIdentifier
    | OpRef SymbolIdentifier
    deriving (Eq, Ord, Show)


-- LISTINGS

-- | A listing of values. Something like (a,b,c) or (..) or (a,b,..)
data Listing a
  = ExplicitListing [Commented a] Bool
  | OpenListing (Commented ())
  | ClosedListing
  deriving (Eq, Show)


-- | A value that can be imported or exported
data Value
    = Value !LowercaseIdentifier
    | OpValue SymbolIdentifier
    | Union (PostCommented UppercaseIdentifier) (Listing UppercaseIdentifier)
    deriving (Eq, Show)

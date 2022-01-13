{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Test.Generators where

import Data.Map.Strict
import Test.QuickCheck

import AST.V0_16
import AST.Module (Module)
import qualified AST.Module
import AST.Structure
import qualified AST.Listing
import Data.Functor.Identity
import qualified Data.Indexed as I
import qualified Reporting.Annotation


capitalLetter :: Gen Char
capitalLetter =
    elements "ABCDEFGHIJKLMNOPQRSTUVWXYZÀΩЉԱႠḀℇ𐐅𝐴𝞨"
    -- Should also be capital, but Data.Char.isUpper does not agree: Ꭳ


capitalAscii :: Gen Char
capitalAscii =
    elements "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


lowerLetter :: Gen Char
lowerLetter =
    elements "abcdefghijklmnopqrstuvwxyz"


number :: Gen Char
number =
    elements "0123456789"


capIdentifier :: Gen UppercaseIdentifier
capIdentifier =
    do
        first <- capitalLetter
        rest <- listOf $ oneof [ capitalLetter, lowerLetter, number ]
        return $ UppercaseIdentifier $ first:rest


lowerIdentifier :: Gen LowercaseIdentifier
lowerIdentifier =
    do
        first <- lowerLetter
        rest <- listOf $ oneof [ capitalLetter, lowerLetter, number ]
        return $ LowercaseIdentifier $ first:rest


commented :: Gen a -> Gen (C2 before after a)
commented inner =
    C ([], []) <$> inner


instance Arbitrary AST.Listing.Value where
    arbitrary =
        do
            name <- capIdentifier
            return $ AST.Listing.Union (C [] name) AST.Listing.ClosedListing


listing :: Gen (AST.Listing.Listing a)
listing =
    return $ AST.Listing.OpenListing (C ([], []) ())


instance Arbitrary (Module [UppercaseIdentifier] (ASTNS Identity [UppercaseIdentifier] 'TopLevelNK)) where
    arbitrary =
        do
            name <- listOf1 $ capIdentifier
            listing <- listing
            moduleType <- fmap (\x -> if x then AST.Module.Port [] else AST.Module.Normal) arbitrary
            return $ AST.Module.Module
                []
                (Just $ AST.Module.Header
                  moduleType
                  (C ([], []) name)
                  Nothing
                  (Just $ C ([], []) listing)
                )
                (Reporting.Annotation.at (Reporting.Annotation.Position 0 0) (Reporting.Annotation.Position 0 0) Nothing)
                (C [] empty)
                (I.Fix2 $ pure $ TopLevel [ Entry $ I.Fix2 $ pure $ CommonDeclaration $ I.Fix2 $ pure $ Definition (I.Fix2 $ pure $ Anything) [] [] (I.Fix2 $ pure $ TupleFunction 2)])

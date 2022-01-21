{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Generators where

import Data.Map.Strict
import Test.QuickCheck

import AST.V0_16
import AST.Structure
import Data.Functor.Identity
import qualified Data.Indexed as I


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


instance Arbitrary ListingValue where
    arbitrary =
        do
            name <- capIdentifier
            return $ Union (C [] name) ClosedListing


listing :: Gen (Listing a)
listing =
    return $ OpenListing (C ([], []) ())


instance Arbitrary (I.Fix2 Identity (ASTNS [UppercaseIdentifier]) 'ModuleNK) where
    arbitrary =
        do
            name <- listOf1 capIdentifier
            listing <- listing
            moduleType <- fmap (\x -> if x then Port [] else Normal) arbitrary
            let body = I.Fix2 $ pure $ ModuleBody
                    [ Entry $ I.Fix2 $ pure $ CommonDeclaration $ I.Fix2 $ pure $ Definition (I.Fix2 $ pure $ Anything) [] [] (I.Fix2 $ pure $ TupleFunction 2)
                    ]
            return $ I.Fix2 $ pure $ Module
                []
                (Just $ I.Fix2 $ pure $ ModuleHeader
                  moduleType
                  (C ([], []) name)
                  Nothing
                  (Just $ C ([], []) listing)
                )
                Nothing
                (C [] empty)
                body

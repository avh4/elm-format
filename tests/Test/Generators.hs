module Test.Generators where

import Test.QuickCheck

import qualified AST.Declaration
import qualified AST.Expression
import qualified AST.Module
import qualified AST.Module.Name
import qualified AST.Pattern
import qualified AST.Variable
import qualified Reporting.Annotation
import qualified Reporting.Region


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


capIdentifier :: Gen String
capIdentifier =
    do
        first <- capitalLetter
        rest <- listOf $ oneof [ capitalLetter, lowerLetter, number ]
        return $ first:rest


lowerIdentifier :: Gen String
lowerIdentifier =
    do
        first <- lowerLetter
        rest <- listOf $ oneof [ capitalLetter, lowerLetter, number ]
        return $ first:rest


nowhere :: Reporting.Region.Position
nowhere =
    Reporting.Region.Position 0 0


located :: a -> Reporting.Annotation.Located a
located =
    Reporting.Annotation.at nowhere nowhere


instance Arbitrary Reporting.Region.Region where
    arbitrary =
        return $ Reporting.Region.Region nowhere nowhere


instance (Arbitrary ann, Arbitrary a) => Arbitrary (Reporting.Annotation.Annotated ann a) where
    arbitrary =
        do
            ann <- arbitrary
            a <- arbitrary
            return $ Reporting.Annotation.A ann a


instance Arbitrary AST.Variable.Value where
    arbitrary =
        do
            name <- capIdentifier
            return $ AST.Variable.Alias name


instance (Arbitrary a) => Arbitrary (AST.Variable.Listing a) where
    arbitrary =
        do
            vars <- listOf arbitrary
            case vars of
                [] -> return $ AST.Variable.Listing [] True
                _ -> return $ AST.Variable.Listing vars False


instance Arbitrary AST.Module.Module where
    arbitrary =
        do
            name <- listOf1 $ capIdentifier
            listing <- arbitrary
            return $ AST.Module.Module
                name
                (located Nothing)
                listing
                []
                [ AST.Declaration.Decl $ located $ AST.Declaration.Definition $ located $ AST.Expression.Definition (located $ AST.Pattern.Anything) [] (located $ AST.Expression.TupleFunction 1) True]

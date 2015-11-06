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


nowhere :: Reporting.Region.Position
nowhere =
    Reporting.Region.Position 0 0


located :: a -> Reporting.Annotation.Located a
located =
    Reporting.Annotation.at nowhere nowhere


instance Arbitrary AST.Module.Module where
    arbitrary =
        return $ AST.Module.Module
            ["Main"]
            (located Nothing)
            (AST.Variable.Listing [] True)
            []
            [ AST.Declaration.Decl $ located $ AST.Declaration.Definition $ located $ AST.Expression.Definition (located $ AST.Pattern.Anything) [] (located $ AST.Expression.TupleFunction 1) True]

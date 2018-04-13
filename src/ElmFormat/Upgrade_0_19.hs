{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Upgrade_0_19 (transform) where

import AST.V0_16
import AST.Expression
import AST.Pattern
import AST.Variable
import Reporting.Annotation (Located(A))

import qualified Reporting.Annotation as RA
import qualified Reporting.Region as Region


transform :: Expr -> Expr
transform expr =
    case RA.drop expr of
        VarExpr var | isBasics "flip" var ->
            noRegion $ Lambda
                [makeArg "f", makeArg "b", makeArg "a"] []
                (noRegion $ App
                    (makeVarRef "f")
                    [([], makeVarRef "a"), ([], makeVarRef "b")]
                    (FAJoinFirst JoinAll)
                )
                False

        App (A _ (VarExpr var)) [(pre, arg1)] _ | isBasics "flip" var ->
            noRegion $ Lambda
                [makeArg "b", makeArg "a"] pre
                (noRegion $ App
                    arg1
                    [([], makeVarRef "a"), ([], makeVarRef "b")]
                    (FAJoinFirst JoinAll)
                )
                False

        App (A _ (VarExpr var)) [(pre, arg1), arg2] _ | isBasics "flip" var ->
            noRegion $ Lambda
                [makeArg "a"] pre
                (noRegion $ App
                    arg1
                    [([], makeVarRef "a"), arg2]
                    (FAJoinFirst JoinAll)
                )
                False

        App (A _ (VarExpr var)) ((pre, arg1):arg2:arg3:argRest) multiline | isBasics "flip" var ->
            noRegion $ Parens
                (Commented pre (noRegion $ App arg1 (arg3:arg2:argRest) multiline) [])

        _ ->
            expr


--
-- Generic helpers
--


nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> RA.Located a
noRegion =
    RA.at nowhere nowhere


isBasics :: String -> Ref -> Bool
isBasics targetName var =
    case var of
        VarRef [] (LowercaseIdentifier name) | name == targetName -> True
        VarRef [(UppercaseIdentifier "Basics")] (LowercaseIdentifier name) | name == targetName -> True
        _ -> False


makeArg :: String -> (Comments, Pattern)
makeArg varName =
    ([], noRegion $ VarPattern $ LowercaseIdentifier varName)


makeVarRef :: String -> Expr
makeVarRef varName =
    noRegion $ VarExpr $ VarRef [] $ LowercaseIdentifier varName

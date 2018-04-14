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
        --
        -- Basics.flip
        --

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

        --
        -- Basics.curry
        --

        VarExpr var | isBasics "curry" var ->
            noRegion $ Lambda
                [makeArg "f", makeArg "a", makeArg "b"] []
                (noRegion $ App
                    (makeVarRef "f")
                    [([], noRegion $ AST.Expression.Tuple
                         [ Commented [] (makeVarRef "a") []
                         , Commented [] (makeVarRef "b") []
                         ] False
                    )]
                    (FAJoinFirst JoinAll)
                )
                False

        App (A _ (VarExpr var)) [(pre1, arg1)] _ | isBasics "curry" var ->
            noRegion $ Lambda
                [makeArg "a", makeArg "b"] pre1
                (noRegion $ App arg1
                    [([], noRegion $ AST.Expression.Tuple
                        [ Commented [] (makeVarRef "a") []
                        , Commented [] (makeVarRef "b") []
                        ] False
                    )]
                    (FAJoinFirst JoinAll)
                )
                False

        App (A _ (VarExpr var)) [(pre1, arg1),(pre2, arg2)] _ | isBasics "curry" var ->
            noRegion $ Lambda
                [makeArg "b"] pre1
                (noRegion $ App arg1
                    [([], noRegion $ AST.Expression.Tuple
                        [ Commented pre2 arg2 []
                        , Commented [] (makeVarRef "b") []
                        ] False
                    )]
                    (FAJoinFirst JoinAll)
                )
                False

        App (A _ (VarExpr var)) ((pre1, arg1):(pre2, arg2):(pre3, arg3):argRest) multiline | isBasics "curry" var ->
            noRegion $ App arg1
                ((pre1, noRegion $ AST.Expression.Tuple
                     [ Commented pre2 arg2 []
                     , Commented pre3 arg3 []
                     ] False
                ):argRest)
                multiline

        --
        -- Basics.uncurry
        --

        VarExpr var | isBasics "uncurry" var ->
            noRegion $ Lambda
                [makeArg "f", ([], noRegion $ AST.Pattern.Tuple [makeArg' "a", makeArg' "b"]) ] []
                (noRegion $ App
                    (makeVarRef "f")
                    [ ([], makeVarRef "a")
                    , ([], makeVarRef "b")
                    ]
                    (FAJoinFirst JoinAll)
                )
                False

        App (A _ (VarExpr var)) [(pre1, arg1)] _ | isBasics "uncurry" var ->
            noRegion $ Lambda
                [([], noRegion $ AST.Pattern.Tuple [makeArg' "a", makeArg' "b"]) ] pre1
                (noRegion $ App arg1
                    [ ([], makeVarRef "a")
                    , ([], makeVarRef "b")
                    ]
                    (FAJoinFirst JoinAll)
                )
                False

        App (A _ (VarExpr var)) ((pre1, arg1):(preT, A _ (AST.Expression.Tuple [Commented preA exprA postA, Commented preB exprB postB] multiline)):extraArgs) _ | isBasics "uncurry" var ->

            noRegion $ Parens $
                Commented (pre1 ++ preT)
                    (noRegion $ App arg1
                        ((preA ++ postA, exprA):(preB ++ postB, exprB):extraArgs)
                        (if multiline then FASplitFirst else (FAJoinFirst JoinAll))
                    )
                    []

        App (A _ (VarExpr var)) ((pre1, arg1):extraArgs) multiline | isBasics "uncurry" var ->
            let
                newMultiline =
                    case multiline of
                        FASplitFirst -> FASplitFirst
                        FAJoinFirst SplitAll -> FASplitFirst
                        FAJoinFirst JoinAll -> FAJoinFirst JoinAll
            in
            noRegion $ App
                (noRegion $ Lambda
                    [([], noRegion $ AST.Pattern.Tuple [makeArg' "a", makeArg' "b"]) ] pre1
                    (noRegion $ App arg1
                        [ ([], makeVarRef "a")
                        , ([], makeVarRef "b")
                        ]
                        (FAJoinFirst JoinAll)
                    )
                    False
                )
                extraArgs
                newMultiline

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


makeArg' :: String -> Commented Pattern
makeArg' varName =
    Commented [] (noRegion $ VarPattern $ LowercaseIdentifier varName) []


makeVarRef :: String -> Expr
makeVarRef varName =
    noRegion $ VarExpr $ VarRef [] $ LowercaseIdentifier varName

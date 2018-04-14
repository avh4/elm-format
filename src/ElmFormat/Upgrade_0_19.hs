{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
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
    let
        flipLambda :: Expr
        flipLambda =
            noRegion $ Lambda
                [makeArg "f", makeArg "b", makeArg "a"] []
                (noRegion $ App
                    (makeVarRef "f")
                    [([], makeVarRef "a"), ([], makeVarRef "b")]
                    (FAJoinFirst JoinAll)
                )
                False
    in
    case RA.drop expr of
        --
        -- Basics.flip
        --

        VarExpr var | isBasics "flip" var ->
            flipLambda

        App (A _ (VarExpr var)) args multiline | isBasics "flip" var ->
            applyLambda flipLambda args multiline

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


class MapExpr a where
    mapExpr :: (Expr' -> Expr') -> a -> a

instance MapExpr (Located Expr') where
    mapExpr f (A region a) = A region (f a)

instance MapExpr IfClause where
    mapExpr f (cond, body) = (mapExpr f cond, mapExpr f body)

instance MapExpr a => MapExpr (PreCommented a) where
    mapExpr f (pre, a) = (pre, mapExpr f a)

instance MapExpr a => MapExpr (WithEol a) where
    mapExpr f (a, eol) = (mapExpr f a, eol)

instance MapExpr a => MapExpr [a] where
    mapExpr f list = fmap (mapExpr f) list

instance MapExpr a => MapExpr (a, Bool) where
    mapExpr f (a, b) = (mapExpr f a, b)

instance MapExpr a => MapExpr (Commented Pattern, a) where
    mapExpr f (x, a) = (x, mapExpr f a)

instance MapExpr a => MapExpr (Comments, Ref, Comments, a) where
    mapExpr f (pre, op, post, a) = (pre, op, post, mapExpr f a)

instance MapExpr a => MapExpr (Commented a) where
    mapExpr f (Commented pre e post) = Commented pre (mapExpr f e) post

instance MapExpr a => MapExpr (Pair key a) where
    mapExpr f (Pair key value multi) = Pair key (mapExpr f value) multi

instance MapExpr Expr' where
  mapExpr f expr =
    case expr of
        Unit _ -> expr
        AST.Expression.Literal _ -> expr
        VarExpr _ -> expr

        App f' args multiline ->
            App (mapExpr f f') (mapExpr f args) multiline
        Unary op e ->
            Unary op (mapExpr f e)
        Binops left restOps multiline ->
            Binops (mapExpr f left) (mapExpr f restOps) multiline
        Parens e ->
            Parens (mapExpr f e)
        ExplicitList terms' post multiline ->
            ExplicitList (mapExpr f terms') post multiline
        Range e1 e2 multiline ->
            Range (mapExpr f e1) (mapExpr f e2) multiline
        AST.Expression.Tuple es multiline ->
            AST.Expression.Tuple (mapExpr f es) multiline
        TupleFunction _ -> expr
        AST.Expression.Record b fs post multiline ->
            AST.Expression.Record b (mapExpr f fs) post multiline
        Access e field' ->
            Access (mapExpr f e) field'
        AccessFunction _ -> expr
        Lambda params pre body multi ->
            Lambda params pre (mapExpr f body) multi
        If c1 elseIfs els ->
            If (mapExpr f c1) (mapExpr f elseIfs) (mapExpr f els)
        Let decls pre body ->
            Let (mapExpr f decls) pre body
        Case cond branches ->
            Case (mapExpr f cond) (mapExpr f branches)
        GLShader _ -> expr

instance MapExpr LetDeclaration where
  mapExpr f d =
      case d of
          LetDefinition name args pre body -> LetDefinition name args pre (mapExpr f body)
          _ -> d


inlineVar :: LowercaseIdentifier -> Expr' -> Expr' -> Expr'
inlineVar name value expr =
    case expr of
        VarExpr (VarRef [] n) | n == name -> value
        _ -> mapExpr (inlineVar name value) expr


applyLambda :: Expr -> [PreCommented Expr] -> FunctionApplicationMultiline -> Expr
applyLambda lambda args appMultiline =
    case (RA.drop lambda, args) of
        (Lambda ((preVar, A _ (VarPattern name)):[]) preBody body _, (pre, arg):restArgs) ->
            noRegion $ App (noRegion $ Parens $ Commented preBody (mapExpr (inlineVar name (Parens $ Commented (preVar ++ pre) arg [])) body) []) restArgs appMultiline

        (Lambda ((preVar, A _ (VarPattern name)):restVar) preBody body multiline, (pre, arg):restArgs) ->
            applyLambda (noRegion $ Lambda restVar preBody (mapExpr (inlineVar name (Parens $ Commented (preVar ++ pre) arg [])) body) multiline) restArgs appMultiline

        (_, []) -> lambda

        _ -> noRegion $ App lambda args appMultiline

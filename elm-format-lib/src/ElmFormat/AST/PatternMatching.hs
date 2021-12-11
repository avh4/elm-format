{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module ElmFormat.AST.PatternMatching where

import ElmFormat.AST.Shared
import AST.V0_16
import AST.Structure
import Reporting.Annotation (Located(At))
import Data.Indexed as I


{-| Takes a list of patterns and matches them agains a type,
extracting the types of each of the patterns
and the return type of a function that has that list of patterns as its parameters.

TODO: retain all comments in the output
TODO: make complete function so it doesn't crash on invalid source files
-}
matchType ::
    List (C1 'BeforeTerm (ASTNS Located ns 'PatternNK))
    -> ASTNS Located ns 'TypeNK
    -> ( List (C1 'BeforeTerm (ASTNS Located ns 'PatternNK), ASTNS Located ns 'TypeNK)
       , ASTNS Located ns 'TypeNK
       )
matchType [] typ = ( [], typ )
matchType (pat : restPat) (I.Fix (At region (FunctionType (C eol typ) restTyp multiline))) =
    let
        nextTyp =
            case toCommentedList restTyp of
                [ (C _ single) ] -> single
                ( (C (_, _, eol2) first) : rest ) -> I.Fix $ At region $ FunctionType (C eol2 first) (Sequence rest) multiline

        ( pats, retType ) =
            matchType restPat nextTyp
    in
    ( (pat, typ) : pats, retType )

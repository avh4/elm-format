{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AST.Annotated where

import AST.Declaration hiding (TopLevelStructure)
import qualified AST.Declaration
import AST.Expression hiding (Expression)
import Data.Fix
import ElmFormat.Mapping


type Expression ns ann =
    Fix (AnnotatedExpression ns ann)


type TopLevelStructure ns ann =
    AST.Declaration.TopLevelStructure (Declaration ns (Expression ns ann))


instance MapNamespace a b (TopLevelStructure a ann) (TopLevelStructure b ann) where
    mapNamespace f =
        let
            x d =
              d''
              where
                  d' :: Declaration a (Fix (AnnotatedExpression b ann))
                  d' = fmap (mapNamespace f) d

                  d'' :: Declaration b (Fix (AnnotatedExpression b ann))
                  d'' = mapNamespace f d'
        in
        fmap x

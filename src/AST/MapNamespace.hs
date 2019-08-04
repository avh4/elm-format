module AST.MapNamespace where

import           AST.Declaration
import           AST.Expression
import           AST.MapExpr
import           AST.Variable
import           AST.V0_16
import           Reporting.Annotation


class MapNamespace a where
    mapNamespace :: ([UppercaseIdentifier] -> [UppercaseIdentifier]) -> a -> a


instance MapNamespace Expr' where
  mapNamespace f expr = case expr of
    VarExpr var -> VarExpr (mapNamespace f var)

    -- TODO: map references in patterns

    _           -> mapExpr (mapNamespace f) expr


instance MapNamespace Declaration where
  mapNamespace f decl = case decl of
        -- TODO: map references in patterns
    Definition first rest comments expr ->
      Definition first rest comments (mapNamespace f expr)

    PortDefinition name comments expr ->
      PortDefinition name comments (mapNamespace f expr)

    _ -> decl


instance MapNamespace Ref where
  mapNamespace f ref = case ref of
    VarRef namespace name -> VarRef (f namespace) name
    TagRef namespace name -> TagRef (f namespace) name
    OpRef name            -> OpRef name


instance MapNamespace a => MapNamespace [a] where
  mapNamespace f list = fmap (mapNamespace f) list


instance MapNamespace a => MapNamespace (TopLevelStructure a) where
  mapNamespace f struct = case struct of
    Entry a -> Entry $ mapNamespace f a
    _       -> struct

instance MapNamespace a => MapNamespace (Located a) where
  mapNamespace f (A region a) = A region (mapNamespace f a)

module AST.Type
    ( Type, Type'(..)
    , Port(..), getPortType
    , fieldMap
    , tuple
    ) where

import qualified Data.Map as Map

import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


-- DEFINITION

type Type =
    A.Located Type'


data Type'
    = RLambda Type Type
    | RVar String
    | RType Var.Ref
    | RApp Type [Type]
    | RRecord [(String, Type)] (Maybe Type)
    deriving (Show)


data Port
    = Normal Type
    | Signal { root :: Type, arg :: Type }
    deriving (Show)


getPortType :: Port -> Type
getPortType portType =
  case portType of
    Normal tipe -> tipe
    Signal tipe _ -> tipe


fieldMap :: [(String,a)] -> Map.Map String [a]
fieldMap fields =
  let add r (field,tipe) =
        Map.insertWith (++) field [tipe] r
  in
      foldl add Map.empty fields


tuple :: R.Region -> [Type] -> Type
tuple region types =
  let name = Var.VarRef ("_Tuple" ++ show (length types))
  in
      A.A region (RApp (A.A region (RType name)) types)

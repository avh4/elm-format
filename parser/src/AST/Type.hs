module AST.Type
    ( Type, Type'(..)
    , Port(..), getPortType
    , fieldMap
    , tuple
    ) where

import qualified Data.Map as Map

import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


-- DEFINITION

type Type =
    A.Located Type'


data Type'
    = RVar String
    | RTupleFunction Int -- will be 2 or greater, indicating the number of elements in the tuple
    | RApp Type [Type]
    | RTuple [Type]
    | RRecord (Maybe String) [(String, Type, Bool)] Bool
    | RLambda Type [Type]
    deriving (Eq, Show)


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
  A.A region (RTuple types)

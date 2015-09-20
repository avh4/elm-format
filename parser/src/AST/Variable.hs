module AST.Variable where

import Control.Applicative ((<$>), (<*>))
import Data.Binary
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.PrettyPrint as P

import qualified AST.Helpers as Help
import qualified AST.Module.Name as ModuleName
import qualified Reporting.PrettyPrint as P


data Ref
    = VarRef String
    | OpRef String
    deriving (Eq, Ord, Show)


-- LISTINGS

-- | A listing of values. Something like (a,b,c) or (..) or (a,b,..)
data Listing a = Listing
    { _explicits :: [a]
    , _open :: Bool
    }
    deriving (Eq, Ord, Show)


openListing :: Listing a
openListing =
    Listing [] True


closedListing :: Listing a
closedListing =
    Listing [] False


listing :: [a] -> Listing a
listing xs =
    Listing xs False


-- | A value that can be imported or exported
data Value
    = Value !String
    | Alias !String
    | Union !String !(Listing String)
    deriving (Eq, Ord, Show)


-- CATEGORIZING VALUES

getValues :: [Value] -> [String]
getValues values =
  Maybe.mapMaybe getValue values


getValue :: Value -> Maybe String
getValue value =
  case value of
    Value name -> Just name
    Alias _ -> Nothing
    Union _ _ -> Nothing


getAliases :: [Value] -> [String]
getAliases values =
  Maybe.mapMaybe getAlias values


getAlias :: Value -> Maybe String
getAlias value =
  case value of
    Value _-> Nothing
    Alias name -> Just name
    Union _ _ -> Nothing


getUnions :: [Value] -> [(String, Listing String)]
getUnions values =
  Maybe.mapMaybe getUnion values


getUnion :: Value -> Maybe (String, Listing String)
getUnion value =
  case value of
    Value _ -> Nothing
    Alias _ -> Nothing
    Union name ctors -> Just (name, ctors)

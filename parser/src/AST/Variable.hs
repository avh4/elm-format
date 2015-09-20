module AST.Variable where

import Control.Applicative ((<$>), (<*>))
import Data.Binary
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.PrettyPrint as P

import qualified AST.Helpers as Help
import qualified AST.Module.Name as ModuleName
import qualified Reporting.PrettyPrint as P


-- RAW NAMES

newtype Raw = Raw String
    deriving (Eq, Ord, Show)


-- TOP LEVEL NAMES

data TopLevel = TopLevelVar
    { topHome :: ModuleName.Canonical
    , topName :: String
    }
    deriving (Eq, Ord, Show)



-- VARIABLE TO STRING

class ToString a where
  toString :: a -> String


instance ToString Raw where
  toString (Raw name) =
      name


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


-- PRETTY VARIABLES

instance P.Pretty Raw where
  pretty _ _ (Raw name) =
      if Help.isOp name
        then P.parens (P.text name)
        else P.text name


instance P.Pretty a => P.Pretty (Listing a) where
  pretty dealiaser _ (Listing explicits open) =
      let dots = [if open then P.text ".." else P.empty]
      in
          P.parens (P.commaCat (map (P.pretty dealiaser False) explicits ++ dots))


instance P.Pretty Value where
  pretty dealiaser _ portable =
    case portable of
      Value name ->
          P.text name

      Alias name ->
          P.text name

      Union name ctors ->
          P.text name <> P.pretty dealiaser False ctors

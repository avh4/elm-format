module AST.Type
    ( Raw, Raw'(..)
    , Port(..), getPortType
    , fieldMap
    , tuple
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Data.Binary
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.PrettyPrint as P

import qualified AST.Variable as Var
import qualified AST.Helpers as Help
import qualified Reporting.Annotation as A
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as R


-- DEFINITION

type Raw =
    A.Located Raw'


data Raw'
    = RLambda Raw Raw
    | RVar String
    | RType Var.Raw
    | RApp Raw [Raw]
    | RRecord [(String, Raw)] (Maybe Raw)
    deriving (Show)


data Port t
    = Normal t
    | Signal { root :: t, arg :: t }
    deriving (Show)


getPortType :: Port tipe -> tipe
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

{--
recordOf :: [(String, Type var)] -> Type var
recordOf fields =
  Record fields Nothing


listOf :: RawType -> RawType
listOf tipe =
  App (Type (Var.Raw "List")) [tipe]
--}

tuple :: R.Region -> [Raw] -> Raw
tuple region types =
  let name = Var.Raw ("_Tuple" ++ show (length types))
  in
      A.A region (RApp (A.A region (RType name)) types)


-- PRETTY PRINTING

instance (P.Pretty t) => P.Pretty (Port t) where
  pretty dealiaser needsParens portType =
    P.pretty dealiaser needsParens (getPortType portType)


instance P.Pretty Raw' where
  pretty dealiaser needsParens tipe =
    case tipe of
      RLambda arg body ->
          P.parensIf needsParens (prettyLambda dealiaser getRawLambda arg body)

      RVar x ->
          P.text x

      RType var ->
          prettyType dealiaser var

      RApp func args ->
          let
            isTuple (A.A _ (RType name)) = Help.isTuple (Var.toString name)
            isTuple _ = False
          in
            prettyApp dealiaser needsParens isTuple func args

      RRecord fields ext ->
          prettyRecord dealiaser (flattenRawRecord fields ext)


-- PRETTY HELPERS

prettyType :: (Var.ToString var) => P.Dealiaser -> var -> P.Doc
prettyType dealiaser var =
  let
    v = Var.toString var
  in
    P.text $
      if v == "_Tuple0" then
        "()"
      else
        maybe v id (Map.lookup v dealiaser)


-- PRETTY LAMBDAS

prettyLambda :: (P.Pretty t) => P.Dealiaser -> (t -> Maybe (t,t)) -> t -> t -> P.Doc
prettyLambda dealiaser getLambda arg body =
  let
    rest =
      gatherLambda getLambda body

    prettyArg t =
      P.pretty dealiaser (Maybe.isJust (getLambda t)) t
  in
    P.sep
      [ prettyArg arg
      , P.sep (map (\t -> P.text "->" <+> prettyArg t) rest)
      ]


getRawLambda :: Raw -> Maybe (Raw, Raw)
getRawLambda (A.A _ tipe) =
  case tipe of
    RLambda arg body -> Just (arg, body)
    _ -> Nothing


gatherLambda :: (t -> Maybe (t,t)) -> t -> [t]
gatherLambda get tipe =
  case get tipe of
    Just (arg, body) ->
        arg : gatherLambda get body

    Nothing ->
        [tipe]


-- PRETTY APP

prettyApp :: (P.Pretty t) => P.Dealiaser -> Bool -> (t -> Bool) -> t -> [t] -> P.Doc
prettyApp dealiaser needsParens isTuple func args
  | isTuple func =
        P.parens $ P.sep $
            P.punctuate P.comma (map (P.pretty dealiaser False) args)

  | null args =
      P.pretty dealiaser needsParens func

  | otherwise =
      P.parensIf needsParens $
        P.hang
          (P.pretty dealiaser True func)
          2
          (P.sep (map (P.pretty dealiaser True) args))


-- PRETTY RECORD

prettyRecord :: (P.Pretty t) => P.Dealiaser -> ([(String, t)], Maybe String) -> P.Doc
prettyRecord dealiaser recordInfo =
  let
    prettyField (field, tipe) =
      P.hang
          (P.text field <+> P.text ":")
          4
          (P.pretty dealiaser False tipe)
  in
  case recordInfo of
    ([], Nothing) ->
        P.text "{}"

    (fields, Nothing) ->
        P.sep
          [ P.cat (zipWith (<+>) (P.lbrace : repeat P.comma) (map prettyField fields))
          , P.rbrace
          ]

    (fields, Just x) ->
        P.hang
            (P.lbrace <+> P.text x <+> P.text "|")
            4
            (P.sep
              [ P.sep (P.punctuate P.comma (map prettyField fields))
              , P.rbrace
              ]
            )


flattenRawRecord
    :: [(String, Raw)]
    -> Maybe Raw
    -> ( [(String, Raw)], Maybe String )
flattenRawRecord fields ext =
  case ext of
    Nothing ->
        (fields, Nothing)

    Just (A.A _ (RVar x)) ->
        (fields, Just x)

    Just (A.A _ (RRecord fields' ext')) ->
        flattenRawRecord (fields' ++ fields) ext'

    _ ->
        error "Trying to flatten ill-formed record."

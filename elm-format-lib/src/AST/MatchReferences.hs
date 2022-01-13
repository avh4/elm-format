module AST.MatchReferences (MatchedNamespace(..), fromMatched, matchReferences, applyReferences) where

import AST.V0_16
import AST.Structure
import Control.Applicative ((<|>))
import Data.Coapplicative
import ElmFormat.ImportInfo (ImportInfo)

import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified Data.Indexed as I


data MatchedNamespace t
    = Local
    | MatchedImport Bool t -- Bool is True if it was originally qualified
    | Unmatched t -- The given namespace is clearly specified, but it is not a known import
    | UnmatchedUnqualified [t] -- An unqualified reference that doesn't match anything known. List is namespaces that we don't know the contents of that are possibilities for a match
    deriving (Eq, Ord, Show, Functor)


fromMatched :: t -> MatchedNamespace t -> t
fromMatched empty Local = empty
fromMatched _ (MatchedImport _ t) = t
fromMatched _ (Unmatched t) = t
fromMatched empty (UnmatchedUnqualified _) = empty


matchReferences ::
    (Coapplicative annf, Ord u) =>
    ImportInfo [u]
    -> I.Fix2 annf (ASTNS [u]) kind
    -> I.Fix2 annf (ASTNS (MatchedNamespace [u])) kind
matchReferences importInfo =
    let
        aliases = Bimap.toMap $ ImportInfo._aliases importInfo
        imports = ImportInfo._directImports importInfo
        exposed = ImportInfo._exposed importInfo
        unresolvedExposingAll = ImportInfo._unresolvedExposingAll importInfo
        unmatchedUnqualified = UnmatchedUnqualified $ Set.toList unresolvedExposingAll

        f locals ns identifier =
            case ns of
                [] ->
                    case Dict.lookup identifier locals of
                        Just () -> Local
                        Nothing ->
                            case Dict.lookup identifier exposed of
                                Nothing -> unmatchedUnqualified
                                Just exposedFrom -> MatchedImport False exposedFrom

                _ ->
                    let
                        self =
                            if Set.member ns imports then
                                Just ns
                            else
                                Nothing

                        fromAlias =
                            Dict.lookup ns aliases

                        resolved =
                            fromAlias <|> self
                    in
                    case resolved of
                        Nothing -> Unmatched ns
                        Just single -> MatchedImport True single

        defineLocal name = Dict.insert name ()

        mapTypeRef locals (ns, u) = (f locals ns (TypeName u), u)
        mapCtorRef locals (ns, u) = (f locals ns (CtorName u), u)
        mapVarRef locals (VarRef ns l) = VarRef (f locals ns (VarName l)) l
        mapVarRef locals (TagRef ns u) = TagRef (f locals ns (CtorName u)) u
        mapVarRef _ (OpRef op) = OpRef op
    in
    topDownReferencesWithContext
        defineLocal
        mapTypeRef mapCtorRef mapVarRef
        mempty


applyReferences ::
    (Coapplicative annf, Ord u) =>
    ImportInfo [u]
    -> I.Fix2 annf (ASTNS (MatchedNamespace [u])) kind
    -> I.Fix2 annf (ASTNS [u]) kind
applyReferences importInfo =
    let
        aliases = Bimap.toMapR $ ImportInfo._aliases importInfo
        exposed = ImportInfo._exposed importInfo
        unresolvedExposingAll = ImportInfo._unresolvedExposingAll importInfo

        f locals ns' identifier =
            case ns' of
                Local -> []
                MatchedImport wasQualified ns ->
                    let
                        qualify =
                            case wasQualified of
                                True ->
                                    (Dict.lookup identifier exposed /= Just ns) -- it's not exposed
                                    || Dict.member identifier locals -- something is locally defined with the same name
                                    || not (Set.null unresolvedExposingAll) -- there's an import with exposing(..) and we can't be sure if something exposed by that would conflict
                                False ->
                                    (Dict.lookup identifier exposed /= Just ns) -- it's not exposed
                                    || Dict.member identifier locals -- something is locally defined with the same name
                    in
                    if qualify
                      then Maybe.fromMaybe ns $ Dict.lookup ns aliases
                      else [] -- This is exposed unambiguously and doesn't need to be qualified
                Unmatched name -> name
                UnmatchedUnqualified _ -> []

        defineLocal name = Dict.insert name ()

        mapTypeRef locals (ns, u) = (f locals ns (TypeName u), u)
        mapCtorRef locals (ns, u) = (f locals ns (CtorName u), u)
        mapVarRef locals (VarRef ns l) = VarRef (f locals ns (VarName l)) l
        mapVarRef locals (TagRef ns u) = TagRef (f locals ns (CtorName u)) u
        mapVarRef _ (OpRef op) = OpRef op
    in
    topDownReferencesWithContext
        defineLocal
        mapTypeRef mapCtorRef mapVarRef
        mempty

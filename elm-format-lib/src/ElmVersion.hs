module ElmVersion where

import Prelude ()
import Relude

import qualified Text.Show


data ElmVersion
  = Elm_0_16 -- TODO: remove 0_16
  | Elm_0_17 -- TODO: remove 0_17
  | Elm_0_18
  | Elm_0_19
  deriving (Eq, Ord)


instance Show ElmVersion where
    show Elm_0_16 = "0.16"
    show Elm_0_17 = "0.17"
    show Elm_0_18 = "0.18"
    show Elm_0_19 = "0.19"


parse :: String -> Either String ElmVersion
parse versionString =
  case versionString of
    "0.17" -> Right Elm_0_17
    "0.18" -> Right Elm_0_18
    "0.19" -> Right Elm_0_19
    _ -> Left ("Invalid Elm version \"" ++ versionString ++ "\".  Supported versions are 0.18, 0.19")


syntax_0_18_disallowLiteralRange :: ElmVersion -> Bool
syntax_0_18_disallowLiteralRange = (>= Elm_0_18)


syntax_0_19_disallowApostropheInVars :: ElmVersion -> Bool
syntax_0_19_disallowApostropheInVars = (>= Elm_0_19)


style_0_19_stringEscape :: ElmVersion -> Bool
style_0_19_stringEscape = (>= Elm_0_19)


style_0_19_cannotExposeOpenListing :: ElmVersion -> Bool
style_0_19_cannotExposeOpenListing = (>= Elm_0_19)

{-# OPTIONS_GHC -Wall #-}
module ElmVersion where


data ElmVersion
  = Elm_0_16 -- TODO: remove 0_16
  | Elm_0_17 -- TODO: remove 0_17
  | Elm_0_18
  | Elm_0_19
  | Elm_0_18_Upgrade
  | Elm_0_19_Upgrade


instance Show ElmVersion where
    show Elm_0_16 = "0.16"
    show Elm_0_17 = "0.17"
    show Elm_0_18 = "0.18"
    show Elm_0_19 = "0.19"
    show Elm_0_18_Upgrade = "0.18"
    show Elm_0_19_Upgrade = "0.19"


parse :: String -> Either String ElmVersion
parse versionString =
  case versionString of
    "0.17" -> Right Elm_0_17
    "0.18" -> Right Elm_0_18
    "0.19" -> Right Elm_0_19
    _ -> Left ("Invalid Elm version \"" ++ versionString ++ "\".  Supported versions are 0.18, 0.19")


syntax_0_19_disallowApostropheInVars :: ElmVersion -> Bool
syntax_0_19_disallowApostropheInVars elmVersion =
    case elmVersion of
        Elm_0_16 -> False
        Elm_0_17 -> False
        Elm_0_18_Upgrade -> False
        Elm_0_18 -> False
        Elm_0_19_Upgrade -> False
        Elm_0_19 -> True


style_0_19_stringEscape :: ElmVersion -> Bool
style_0_19_stringEscape elmVersion =
    case elmVersion of
        Elm_0_16 -> False
        Elm_0_17 -> False
        Elm_0_18 -> False
        Elm_0_18_Upgrade -> False
        _ -> True


style_0_19_cannotExposeOpenListing :: ElmVersion -> Bool
style_0_19_cannotExposeOpenListing elmVersion =
    case elmVersion of
        Elm_0_16 -> False
        Elm_0_17 -> False
        Elm_0_18 -> False
        Elm_0_18_Upgrade -> False
        _ -> True

{-# OPTIONS_GHC -Wall #-}
module ElmVersion where


data ElmVersion
  = Elm_0_16
  | Elm_0_17
  | Elm_0_18


instance Show ElmVersion where
    show Elm_0_16 = "0.16"
    show Elm_0_17 = "0.17"
    show Elm_0_18 = "0.18"


parse :: String -> Either String ElmVersion
parse versionString =
  case versionString of
    "0.16" -> Right Elm_0_16
    "0.17" -> Right Elm_0_17
    "0.18" -> Right Elm_0_18
    _ -> Left ("Invalid Elm version \"" ++ versionString ++ "\".  Supported versions are 0.16, 0.17, 0.18")

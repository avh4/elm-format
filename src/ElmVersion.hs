{-# OPTIONS_GHC -Wall #-}
module ElmVersion where


data ElmVersion
  = Elm_0_16
  | Elm_0_17


instance Show ElmVersion where
    show Elm_0_16 = "0.16"
    show Elm_0_17 = "0.17"


parse :: String -> Either String ElmVersion
parse versionString =
  case versionString of
    "0.16" -> Right Elm_0_16
    "0.17" -> Right Elm_0_17
    _ -> Left ("Invalid Elm version \"" ++ versionString ++ "\".  Supported versions are 0.16, 0.17")

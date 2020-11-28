module Shakefiles.Platform (Platform, platform, zipFormatFor, cabalInstallOs) where

import qualified System.Info


data Platform = Linux | Mac | Windows

instance Show Platform where
    show Linux = "linux-x64"
    show Mac = "mac-x64"
    show Windows = "win-x64"


platform :: Platform
platform =
    case (System.Info.os, System.Info.arch) of
        ("linux", "x86_64") -> Linux
        ("darwin", "x86_64") -> Mac
        ("osx", "x86_64") -> Mac
        ("mingw32", "x86_64") -> Windows
        ("win32", "x86_64") -> Windows
        other -> error ("unhandled operating system: " ++ show other)


zipFormatFor :: Platform -> String
zipFormatFor Linux = "tgz"
zipFormatFor Mac = "tgz"
zipFormatFor Windows = "zip"


cabalInstallOs :: String
cabalInstallOs =
    System.Info.arch ++ "-" ++ os
    where
        os =
            case System.Info.os of
                "darwin" -> "osx"
                "mingw32" -> "windows"
                o -> o

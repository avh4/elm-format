module Shakefiles.Platform (Platform(..), Shakefiles.Platform.all, platform, zipFormatFor, cabalInstallOs, githubRunnerOs, binExt) where

import qualified System.Info


data Platform = Linux | Mac | MacArm64 | Windows

instance Show Platform where
    show Linux = "linux-x64"
    show Mac = "mac-x64"
    show MacArm64 = "mac-arm64"
    show Windows = "win-x64"


all :: [Platform]
all =
    [ Linux
    , Mac
    , MacArm64
    , Windows
    ]


platform :: Platform
platform =
    case (System.Info.os, System.Info.arch) of
        ("linux", "x86_64") -> Linux
        ("darwin", "x86_64") -> Mac
        ("darwin", "aarch64") -> MacArm64
        ("osx", "x86_64") -> Mac
        ("mingw32", "x86_64") -> Windows
        ("win32", "x86_64") -> Windows
        other -> error ("unhandled operating system: " ++ show other)


zipFormatFor :: Platform -> String
zipFormatFor = \case
    Linux -> "tgz"
    Mac -> "tgz"
    MacArm64 -> "tgz"
    Windows -> "zip"


binExt :: Platform -> String
binExt = \case
    Linux -> ""
    Mac -> ""
    MacArm64 -> ""
    Windows -> ".exe"


githubRunnerOs :: Platform -> String
githubRunnerOs = \case
    Linux -> "Linux"
    Windows -> "Windows"
    Mac -> "macOS"
    MacArm64 -> "macOS"


cabalInstallOs :: String
cabalInstallOs =
    System.Info.arch ++ "-" ++ os
    where
        os =
            case System.Info.os of
                "darwin" -> "osx"
                "mingw32" -> "windows"
                o -> o

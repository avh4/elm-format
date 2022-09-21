module Shakefiles.Platform (Platform(..), Shakefiles.Platform.all, platform, zipFormatFor, cabalInstallOs, githubRunnerOs, binExt) where

import qualified System.Info


data Platform = Linux | Mac | Windows | LinuxAarch64

instance Show Platform where
    show Linux = "linux-x64"
    show Mac = "mac-x64"
    show Windows = "win-x64"
    show LinuxAarch64 = "linux-aarch64"


all :: [Platform]
all =
    [ Linux
    , Mac
    , Windows
    , LinuxAarch64
    ]


platform :: Platform
platform =
    case (System.Info.os, System.Info.arch) of
        ("linux", "x86_64") -> Linux
        ("darwin", "x86_64") -> Mac
        ("osx", "x86_64") -> Mac
        ("mingw32", "x86_64") -> Windows
        ("win32", "x86_64") -> Windows
        ("linux", "aarch64") -> LinuxAarch64
        other -> error ("unhandled operating system: " ++ show other)


zipFormatFor :: Platform -> String
zipFormatFor = \case
    Linux -> "tgz"
    Mac -> "tgz"
    Windows -> "zip"
    LinuxAarch64 -> "tgz"


binExt :: Platform -> String
binExt = \case
    Linux -> ""
    Mac -> ""
    Windows -> ".exe"
    LinuxAarch64 -> ""


githubRunnerOs :: Platform -> String
githubRunnerOs = \case
    Linux -> "Linux"
    Windows -> "Windows"
    Mac -> "macOS"
    LinuxAarch64 -> "Linux"


cabalInstallOs :: String
cabalInstallOs =
    System.Info.arch ++ "-" ++ os
    where
        os =
            case System.Info.os of
                "darwin" -> "osx"
                "mingw32" -> "windows"
                o -> o

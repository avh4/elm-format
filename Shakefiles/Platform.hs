module Shakefiles.Platform (Platform(..), Shakefiles.Platform.all, platform, zipFormatFor, cabalInstallOs, ciArchiveLabel, binExt) where

import qualified System.Info


data Platform = Linux | MacX86 | MacArm64 | Windows | LinuxAarch64

instance Show Platform where
    show Linux = "linux-x64"
    show MacX86 = "mac-x64"
    show MacArm64 = "mac-arm64"
    show Windows = "win-x64"
    show LinuxAarch64 = "linux-aarch64"


all :: [Platform]
all =
    [ Linux
    , MacX86
    , MacArm64
    , Windows
    , LinuxAarch64
    ]


platform :: Platform
platform =
    case (System.Info.os, System.Info.arch) of
        ("linux", "x86_64") -> Linux
        ("darwin", "x86_64") -> MacX86
        ("darwin", "aarch64") -> MacArm64
        ("osx", "x86_64") -> MacX86
        ("mingw32", "x86_64") -> Windows
        ("win32", "x86_64") -> Windows
        ("linux", "aarch64") -> LinuxAarch64
        other -> error ("unhandled operating system: " ++ show other)


zipFormatFor :: Platform -> String
zipFormatFor = \case
    Linux -> "tgz"
    MacX86 -> "tgz"
    MacArm64 -> "tgz"
    Windows -> "zip"
    LinuxAarch64 -> "tgz"


binExt :: Platform -> String
binExt = \case
    Linux -> ""
    MacX86 -> ""
    MacArm64 -> ""
    Windows -> ".exe"
    LinuxAarch64 -> ""


ciArchiveLabel :: Platform -> String
ciArchiveLabel = \case
    Linux -> "Linux"
    Windows -> "Windows"
    MacX86 -> "macOS-x86"
    MacArm64 -> "macOS-arm64"
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

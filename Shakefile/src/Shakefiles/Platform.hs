module Shakefiles.Platform (Platform(..), Shakefiles.Platform.all, platform, zipFormatFor, cabalInstallOs, ciArchiveLabel, binExt) where

import qualified System.Info


data Platform
    = LinuxX86
    | LinuxAarch64
    | MacX86
    | MacArm64
    | Windows

instance Show Platform where
    show LinuxX86 = "linux-x64"
    show LinuxAarch64 = "linux-aarch64"
    show MacX86 = "mac-x64"
    show MacArm64 = "mac-arm64"
    show Windows = "win-x64"


all :: [Platform]
all =
    [ LinuxX86
    , LinuxAarch64
    , MacX86
    , MacArm64
    , Windows
    ]


platform :: Platform
platform =
    case (System.Info.os, System.Info.arch) of
        ("linux", "x86_64") -> LinuxX86
        ("linux", "aarch64") -> LinuxAarch64
        ("darwin", "x86_64") -> MacX86
        ("darwin", "aarch64") -> MacArm64
        ("osx", "x86_64") -> MacX86
        ("mingw32", "x86_64") -> Windows
        ("win32", "x86_64") -> Windows
        other -> error ("unhandled operating system: " ++ show other)


zipFormatFor :: Platform -> String
zipFormatFor = \case
    LinuxX86 -> "tgz"
    LinuxAarch64 -> "tgz"
    MacX86 -> "tgz"
    MacArm64 -> "tgz"
    Windows -> "zip"


binExt :: Platform -> String
binExt = \case
    LinuxX86 -> ""
    LinuxAarch64 -> ""
    MacX86 -> ""
    MacArm64 -> ""
    Windows -> ".exe"


ciArchiveLabel :: Platform -> String
ciArchiveLabel = \case
    LinuxX86 -> "Linux-x86"
    LinuxAarch64 -> "Linux-aarch64"
    Windows -> "Windows"
    MacX86 -> "macOS-x86"
    MacArm64 -> "macOS-arm64"


cabalInstallOs :: String
cabalInstallOs =
    System.Info.arch ++ "-" ++ os
    where
        os =
            case System.Info.os of
                "darwin" -> "osx"
                "mingw32" -> "windows"
                o -> o

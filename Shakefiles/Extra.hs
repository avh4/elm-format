module Shakefiles.Extra (phonyPrefix, forEach, addMacOSGhcOptionFFI) where

import Development.Shake
import Data.List (stripPrefix)
import Shakefiles.Platform (platform, Platform(..))


phonyPrefix :: String -> (String -> Action ()) -> Rules ()
phonyPrefix prefix action =
    phonys $ \s -> case stripPrefix prefix s of
        Nothing -> Nothing
        Just match -> Just (action match)



forEach :: Monad m => [a] -> (a -> m ()) -> m ()
forEach list f =
    mapM_ f list


--ghc-option required because of https://gitlab.haskell.org/ghc/ghc/-/issues/20592
addMacOSGhcOptionFFI :: [String] -> [String]
addMacOSGhcOptionFFI options =
    case platform of
        Mac -> ["--ghc-option=-I/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/ffi"] ++ options
        MacArm64 -> ["--ghc-option=-I/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/ffi"] ++ options
        _ -> options
module Shakefiles.Haskell (cabalProject, executable) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified System.Directory
import Shakefiles.Platform (platform)
import qualified Shakefiles.Platform


cabalProject :: String -> [String] -> [String] -> [String] -> [String] -> [String] -> Rules ()
cabalProject name sourceFiles sourcePatterns deps testPatterns testDeps =
    let
        globalConfig =
            [ "cabal.project"
            , "cabal.project.freeze"
            -- , "cabal.project.local"
            ]

        needProjectFiles = do
            sourceFilesFromPatterns <- getDirectoryFiles "" sourcePatterns
            let allFiles = mconcat
                    [ globalConfig
                    , fmap (\d -> "_build/cabal" </> d </> "build.ok") deps
                    , sourceFiles
                    , sourceFilesFromPatterns
                    ]
            need allFiles
            hash <- liftIO $ getHashedShakeVersion allFiles
            return hash
    in
    do
        "_build/cabal/" </> name </> "build.ok" %> \out -> do
            hash <- needProjectFiles
            cmd_ "cabal" "v2-build" "-O0"  (name ++ ":libs") "--enable-tests"
            writeFile' out hash

        (cabalBinPath name "noopt") %> \out -> do
            _ <- needProjectFiles
            cmd_ "cabal" "v2-build" "-O0" (name ++ ":exes") "--enable-tests"

        (cabalBinPath name "opt") %> \out -> do
            _ <- needProjectFiles
            cmd_ "cabal" "v2-build" "-O2" (name ++ ":exes")

        ("_build/docker" </> name) %> \out -> do
            need
                [ "Dockerfile"
                , "package/linux/build-in-docker.sh"
                ]
            _ <- needProjectFiles
            cmd_ "package/linux/build-in-docker.sh"

        "_build/cabal/" </> name </> "test.ok" %> \out -> do
            need globalConfig
            need $ fmap (\d -> "_build/cabal" </> d </> "build.ok") deps
            need $ fmap (\d -> "_build/cabal" </> d </> "build.ok") testDeps
            need sourceFiles
            sourceFilesFromPatterns <- getDirectoryFiles "" sourcePatterns
            need sourceFilesFromPatterns
            testFiles <- getDirectoryFiles "" testPatterns
            need testFiles
            cmd_ "cabal" "v2-test" "-O0" (name ++ ":tests") "--test-show-details=streaming"
            writeFile' out ""


cabalBinPath :: String -> String -> FilePath
cabalBinPath projectName opt =
    let
        version =
            case projectName of
                "elm-format" -> "0.8.5"
                _ -> "0.0.0"
    in
    "dist-newstyle/build" </> Shakefiles.Platform.cabalInstallOs </> "ghc-8.10.3" </> projectName ++ "-" ++ version </> "x" </> projectName </> opt </> "build" </> projectName </> projectName <.> exe


executable :: FilePath -> String -> String -> Rules ()
executable target projectName gitDescribe =
    do
        let zipFormat = Shakefiles.Platform.zipFormatFor platform

        target %> \out -> do
            copyFileChanged (cabalBinPath projectName "noopt") out

        phony ("dist-" ++ projectName) $ need
            [ "dist" </> projectName ++ "-" ++ gitDescribe ++ "-" ++ show platform <.> zipFormat
            ]

        ("_build/dist/" ++ show platform </> projectName <.> exe) %> \out -> do
            let target = takeDirectory1 $ dropDirectory1 $ dropDirectory1 out
            case target of
                "linux-x64" -> do
                    let built = "_build/docker" </> projectName
                    need [ built ]
                    copyFileChanged built out
                _ -> do
                    let binDist = cabalBinPath projectName "opt"
                    need [ binDist ]
                    cmd_ "strip" "-o" out binDist

        ("dist" </> projectName ++ "-" ++ gitDescribe ++ "-" ++ show platform <.> "tgz") %> \out -> do
            let binDir = "_build/dist/" ++ show platform
            need [ binDir </> projectName <.> exe ]
            cmd_ "tar" "zcvf" out "-C" binDir (projectName <.> exe)

        ("dist" </> projectName ++ "-" ++ gitDescribe ++ "-" ++ show platform <.> "zip") %> \out -> do
            let binDir = "_build/dist/" ++ show platform
            let bin = binDir </> projectName <.> exe
            need [ bin ]
            absoluteBinPath <- liftIO $ System.Directory.makeAbsolute bin
            liftIO $ removeFiles "." [ out ]
            cmd_ "7z" "a" "-bb3" "-tzip" "-mfb=258" "-mpass=15" out absoluteBinPath

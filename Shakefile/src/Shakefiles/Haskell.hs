module Shakefiles.Haskell (cabalProject, executable) where

import qualified Data.Aeson.Key as Key
import Data.Char (isSpace)
import Data.List (dropWhileEnd, stripPrefix)
import Data.Yaml (FromJSON, (.:))
import qualified Data.Yaml as Yaml
import Data.Yaml.TH (FromJSON (..))
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Relude.Bool.Guard (whenM)
import Shakefiles.Extra
import Shakefiles.Platform (platform)
import qualified Shakefiles.Platform
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import qualified System.Directory
import Shakefiles.Prelude

docsDir :: FilePath
docsDir = "_build/docs/public"


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
            hashNeed allFiles
    in
    do
        "_build/cabal" </> name </> "version" %> \out -> do
            let packageYaml = name </> "package.yaml"
            need [ packageYaml ]
            (VersionFromYaml version) <- Yaml.decodeFileThrow packageYaml
            writeFileChanged out version

        "_build/cabal" </> name </> "build.ok" %> \out -> do
            hash <- needProjectFiles
            cmd_ "cabal" "v2-build" "-O0" (name ++ ":libs") "--enable-tests"
            writeFile' out hash

        let exeRule variantName cabalFlags = do
              let projectName = name
              let exeName = name
              "_build/bin" </> projectName </> variantName </> exeName <.> exe %> \out -> do
                  _ <- needProjectFiles
                  let cabalTarget = projectName <> ":exe:" <> exeName
                  let builddirFlag = "--builddir=dist-newstyle/_" <> variantName
                  StdoutTrim cabalOutput <- cmd "cabal" "list-bin" builddirFlag cabalFlags cabalTarget
                  cmd_ "cabal" "v2-build" builddirFlag cabalFlags cabalTarget
                  copyFileChanged cabalOutput out

        exeRule "O0" ["-O0", "--enable-tests"]
        exeRule "O2" ["-O2"]
        exeRule "profile"
            [ "-O2"
            , "--enable-profiling"
            , "--enable-executable-profiling"
            , "--enable-library-profiling"
            , "--ghc-options=-fprof-auto -rtsopts"
            ]
        exeRule "coverage" [ "--enable-coverage" ]

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

        "_build/cabal/" </> name </> "haddock.ok" %> \out -> do
            hash <- needProjectFiles
            cmd_ "cabal" "v2-haddock"
                "--haddock-html"
                --"--haddock-for-hackage" -- broken in current haddock
                name
            writeFile' out hash

        "_build/docs/" </> name <.> "ok" %> \out -> do
            let haddockOk = "_build/cabal" </> name </> "haddock.ok"
            need [ haddockOk ]
            buildDir <- cabalBuildDir name
            let docsBuildDir = buildDir </> "doc" </> "html" </> name
            liftIO $ createDirectoryIfMissing True docsDir
            let docsOutDir =  docsDir </> name
            whenM
                (doesDirectoryExist docsOutDir)
                (liftIO $ removeDirectoryRecursive docsOutDir)
            cmd_ "cp" "-r" (docsBuildDir <> "/") docsOutDir
            copyFileChanged haddockOk out


cabalBuildDir :: String -> Action FilePath
cabalBuildDir projectName = do
    version <- readFile' ("_build/cabal" </> projectName </> "version")
    return $ "dist-newstyle" </> "build" </> Shakefiles.Platform.cabalInstallOs </> "ghc-9.6.7" </> projectName ++ "-" ++ version


executable :: String -> String -> String -> Rules ()
executable projectName exeName gitDescribe =
    do
        phony ("dist-" ++ projectName) $ need
            [ "dist" </> projectName ++ "-" ++ gitDescribe ++ "-" ++ show platform <.> Shakefiles.Platform.zipFormatFor platform
            ]

        ("_build" </> "dist" </> show platform </> exeName <.> exe) %> \out -> do
            let binDist = "_build" </> "bin" </> "elm-format" </> "O2" </> "elm-format" <.> exe
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

        phonyPrefix (projectName ++ "-publish-") $ \version ->
            need $
                concatMap (\target ->
                    [ "publish" </> version </> projectName ++ "-" ++ version ++ "-" ++ show target <.> Shakefiles.Platform.zipFormatFor target
                    , "publish" </> version </> projectName ++ "-" ++ version ++ "-" ++ show target <.> Shakefiles.Platform.zipFormatFor target <.> "asc"
                    ]
                )
                Shakefiles.Platform.all

        let buildInDocker =
                []
        let buildViaNix =
                [ Shakefiles.Platform.LinuxX86
                , Shakefiles.Platform.LinuxAarch64
                ]
        let buildOnCi =
                [ Shakefiles.Platform.Windows
                , Shakefiles.Platform.MacX86
                , Shakefiles.Platform.MacArm64
                ]

        forEach buildInDocker $ \target -> do
            let zipExt = Shakefiles.Platform.zipFormatFor target

            ("_build" </> "docker" </> "*" </> show target </> projectName) %> \out -> do
                need
                    [ "package/linux/build-in-docker.sh"
                    ]
                let sha = takeDirectory1 $ dropDirectory1 $ dropDirectory1 out
                cmd_ "package/linux/build-in-docker.sh" sha

            ("publish" </> "*" </> projectName ++ "-*-" ++ show target <.> zipExt) %> \out -> do
                let tag = takeDirectory1 $ dropDirectory1 out
                StdoutTrim sha <- cmd "git" "rev-list" "-n1" ("tags/" ++ tag)
                let binDir = "_build" </> "docker" </> sha </> show target
                let binFile = projectName ++ Shakefiles.Platform.binExt target
                need [ binDir </> binFile ]
                cmd_ "tar" "zcvf" out "-C" binDir binFile

        forEach buildViaNix $ \target -> do
            let zipExt = Shakefiles.Platform.zipFormatFor target

            ("publish" </> "*" </> projectName ++ "-*-" ++ show target <.> zipExt) %> \out -> do
                let tag = takeDirectory1 $ dropDirectory1 out
                StdoutTrim sha <- cmd "git" "rev-list" "-n1" ("tags/" ++ tag)
                let binDir = "_build" </> "nix-build" </> "from-git" </> sha </> "out" </> "default" </> "dist." <> show target </> "bin"
                let binFile = projectName ++ Shakefiles.Platform.binExt target
                need [ binDir </> binFile ]
                cmd_ "tar" "zcvf" out "-C" binDir binFile

        forEach buildOnCi $ \target -> do
            let ciArchiveLabel = Shakefiles.Platform.ciArchiveLabel target
            let zipExt = Shakefiles.Platform.zipFormatFor target

            [ "_build" </> "github-ci" </> "unzipped" </> projectName ++ "-*-" ++ show target <.> zipExt,
              "_build" </> "github-ci" </> "unzipped" </> projectName ++ "-*-" ++ show target <.> zipExt <.> "minisig"
              ] &%> \[zip, sig] -> do
                let pubkey = "keys/github-actions.pub"
                need [ pubkey ]
                let outDir = takeDirectory zip
                let tag = drop (length projectName + 1) $ (reverse . drop (length (show target) + 1) . reverse) $ dropExtension $ takeFileName zip
                StdoutTrim sha <- cmd "git" "rev-list" "-n1" ("tags/" ++ tag)
                let ciArchive = "downloads" </> projectName ++ "-" ++ sha ++ "-" ++ ciArchiveLabel <.> "zip"
                need [ ciArchive ]
                liftIO $ removeFiles "." [ zip, sig ]
                cmd_ "unzip" "-o" "-d" outDir ciArchive
                cmd_ "minisign" "-V" "-p" pubkey "-x" sig "-m" zip

            "publish" </> "*" </> projectName ++ "-*-" ++ show target <.> zipExt %> \out -> do
                let source = "_build" </> "github-ci" </> "unzipped" </> takeFileName out
                copyFileChanged source out


newtype VersionFromYaml =
  VersionFromYaml String

instance FromJSON VersionFromYaml where
  parseJSON = Yaml.withObject "hpack" $ \o -> VersionFromYaml
    <$> o .: Key.fromString "version"

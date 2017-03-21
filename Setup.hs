import Distribution.Simple
import Distribution.Simple.BuildPaths (autogenModulesDir)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Setup (BuildFlags)
import Distribution.PackageDescription (PackageDescription, emptyHookedBuildInfo)
import System.FilePath ((</>))
import System.Process (readProcess)


main = defaultMainWithHooks $ simpleUserHooks { buildHook = myBuildHook }


myBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuildHook packageDescription buildInfo userHooks buildFlags =
    do
        writeCustomFile (autogenModulesDir buildInfo </> "Build_elm_format.hs")
        buildHook simpleUserHooks packageDescription buildInfo userHooks buildFlags


writeCustomFile :: FilePath -> IO ()
writeCustomFile filepath = do
  putStrLn $ "Generating " ++ filepath ++ "..."

  desc <- readProcess "git" ["describe", "--dirty=-modified"] ""
  now <- readProcess "date" ["+%s"] ""

  writeFile filepath $ unlines
      [ "module Build_elm_format where"
      , ""
      , "gitDescribe :: String"
      , "gitDescribe = " ++ show (init desc)
      ]

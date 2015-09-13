{-| This script builds any version of the Elm Platform from source.
Before you use it, make sure you have the Haskell Platform with a recent
version of cabal.

To install a released version of Elm, you will run something like this:

    runhaskell BuildFromSource.hs 0.15.1

Before you do that, in some directory of your choosing, add
wherever/Elm-Platform/0.15.1/.cabal-sandbox/bin to your PATH.

Then, run the above. You will now actually have a new directory for the
Elm Platform, like this:

    Elm-Platform/0.15.1/
        elm-make/        -- git repo for the build tool, ready to edit
        elm-repl/        -- git repo for the REPL, ready to edit
        ...
        .cabal-sandbox/  -- various build files

All of the executables you need are in .cabal-sandbox/bin, which is on
your PATH and thus can be used from anywhere.

You can build many versions of the Elm Platform, so it is possible to have
Elm-Platform/0.15.1/ and Elm-Platform/0.13/ with no problems. It is up to you
to manage your PATH variable or symlinks though.

To get set up with the master branch of all Elm Platform projects, run this:

    runhaskell BuildFromSource.hs master

From there you can start developing on any of the projects, switching branches
and testing interactions between projects.
-}
module Main where

import qualified Data.List          as List
import qualified Data.Map           as Map
import           System.Directory   (createDirectoryIfMissing,
                                     getCurrentDirectory, setCurrentDirectory)
import           System.Environment (getArgs)
import           System.Exit        (ExitCode, exitFailure)
import           System.FilePath    ((</>))
import           System.IO          (hPutStrLn, stderr)
import           System.Process     (rawSystem)


(=:) = (,)

configs :: Map.Map String [(String, (String, String))]
configs =
  Map.fromList
    [
      "build" =:
        [ "elm-compiler" =: ("avh4", "elm-format")
        ]
    ]


main :: IO ()
main =
 do args <- getArgs
    case args of
      [version] | Map.member version configs ->
          let artifactDirectory = "Elm-Platform" </> version
              repos = configs Map.! version
          in
              makeRepos artifactDirectory version repos

      _ ->
        do hPutStrLn stderr $
               "Expecting one of the following values as an argument:\n" ++
               "    " ++ List.intercalate ", " (Map.keys configs)
           exitFailure


makeRepos :: FilePath -> String -> [(String, (String, String))] -> IO ()
makeRepos artifactDirectory version repos =
 do createDirectoryIfMissing True artifactDirectory
    setCurrentDirectory artifactDirectory
    root <- getCurrentDirectory
    mapM_ (uncurry (makeRepo root)) repos

    cabal [ "update" ]

    -- create a sandbox for installation
    cabal [ "sandbox", "init" ]

    -- add each of the sub-directories as a sandbox source
    cabal ([ "sandbox", "add-source" ] ++ map fst repos)

    -- install all of the packages together in order to resolve transitive dependencies robustly
    -- (install the dependencies a bit more quietly than the elm packages)
    cabal ([ "install", "-j", "--only-dependencies", "--ghc-options=\"-w\"" ] ++ map fst repos)
    cabal ([ "install", "-j", "--ghc-options=\"-XFlexibleContexts\"" ] ++ filter (/= "elm-reactor") (map fst repos))

    -- elm-reactor needs to be installed last because of a post-build dependency on elm-make
    cabal ([ "install", "-j", "elm-reactor" ] ++ if version <= "0.15.1" then [ "--constraint=fsnotify<0.2" ] else [])

    return ()

makeRepo :: FilePath -> String -> (String, String) -> IO ()
makeRepo root projectName (user,version) =
  do  -- get the right version of the repo
    git [ "clone", "https://github.com/" ++ user ++ "/" ++ projectName ++ ".git" ]
    setCurrentDirectory projectName
    git [ "checkout", version, "--quiet" ]

    -- move back into the root
    setCurrentDirectory root


-- HELPER FUNCTIONS

cabal :: [String] -> IO ExitCode
cabal = rawSystem "cabal"

git :: [String] -> IO ExitCode
git = rawSystem "git"

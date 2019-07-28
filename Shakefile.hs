import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions $ do
    Stdout path <- liftIO $ cmd "stack path --local-install-root"

    let elmFormat = takeWhile (/= '\n') path </> "bin/elm-format" <.> exe

    want [ "test" ]

    phony "test" $ do
        need [ elmFormat ]
        cmd_ "bash tests/run-tests.sh"

    phony "clean" $ do
        cmd_ "stack clean"

    elmFormat %> \out ->
        cmd_ "stack build"

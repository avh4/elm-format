cd elm-format
rmdir /s /q .cabal-sandbox
del /s /q cabal.sandbox.config
cabal sandbox init
cabal update
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
cabal test
cabal build
type dist\test\elm-format-0.3.0-elm-format-tests.log


# brew install wine

curl -O 'https://www.haskell.org/cabal/release/cabal-install-1.24.0.2/cabal-install-1.24.0.2-i386-unknown-mingw32.zip'
curl -O 'http://downloads.haskell.org/~ghc/8.0.1/ghc-8.0.1-i386-unknown-mingw32.tar.xz'

# unzip cabal, ghc

function wine_cabal {
  wine cabal.exe --with-ghc=ghc-8.0.1/bin/ghc.exe
}

wine_cabal update
wine_cabal install --only-dependencies
wine_cabal build
# wine_cabal install --enable-test --only-dependencies
# wine_cabal test

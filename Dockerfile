FROM buildpack-deps:latest

ENV GHC_VERSION 8.8.4

ENV DEBIAN_FRONTEND noninteractive
ENV LANG C.UTF-8

RUN curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/local/bin/ghcup && \
    chmod +x /usr/local/bin/ghcup
RUN ghcup install cabal recommended
RUN ghcup install ghc $GHC_VERSION && \
    ghcup set ghc $GHC_VERSION
ENV PATH $PATH:/root/.ghcup/bin


# Install elm-format dependencies
COPY cabal.project ./
COPY cabal.project.freeze ./
COPY elm-format.cabal ./
RUN cabal v2-update
RUN cabal v2-install --lib shake

RUN cabal v2-build --only-dependencies
RUN cabal v2-build --only-dependencies --enable-tests

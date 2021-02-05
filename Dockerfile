FROM alpine:latest as build

ENV GHC_VERSION 8.10.3

ENV LANG C.UTF-8

# Install ghc and cabal-isntall
RUN apk add --no-cache curl
RUN curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/local/bin/ghcup && \
    chmod +x /usr/local/bin/ghcup
RUN ghcup install cabal recommended
RUN apk add --no-cache \
        autoconf \
        gcc \
        gmp \
        gmp-dev \
        libffi \
        libffi-dev \
        llvm10 \
        make \
        musl-dev \
        ncurses-dev \
        ncurses-static \
        wget \
        zlib-dev \
        zlib-static
RUN ghcup install ghc $GHC_VERSION && \
    ghcup set ghc $GHC_VERSION
ENV PATH $PATH:/root/.ghcup/bin

WORKDIR /elm-format

# Install elm-format dependencies
COPY cabal.project ./
COPY cabal.project.freeze ./

COPY elm-format-markdown/elm-format-markdown.cabal elm-format-markdown/
COPY elm-format-lib/elm-format-lib.cabal elm-format-lib/
COPY elm-format-test-lib/elm-format-test-lib.cabal elm-format-test-lib/
COPY elm-format.cabal ./

RUN cabal v2-update

RUN cabal v2-build --ghc-option=-optl=-static --ghc-option=-split-sections -O2 elm-format-markdown --only-dependencies
COPY elm-format-markdown elm-format-markdown
RUN cabal v2-build --ghc-option=-optl=-static --ghc-option=-split-sections -O2 elm-format-markdown

RUN cabal v2-build --ghc-option=-optl=-static --ghc-option=-split-sections -O2 elm-format-lib --only-dependencies
COPY elm-format-lib elm-format-lib
RUN cabal v2-build --ghc-option=-optl=-static --ghc-option=-split-sections -O2 elm-format-lib

# RUN cabal v2-install --lib shake
# RUN cabal v2-build --only-dependencies
# RUN cabal v2-build --only-dependencies --enable-tests
# RUN cabal v2-install ShellCheck
RUN cabal v2-build --ghc-option=-optl=-static --ghc-option=-split-sections -O2 --only-dependencies

# Build elm-format
COPY src src
COPY generated generated
RUN cabal v2-build --ghc-option=-optl=-static --ghc-option=-split-sections -O2
RUN cp dist-newstyle/build/x86_64-linux/ghc-*/elm-format-*/x/elm-format/opt/build/elm-format/elm-format ./
RUN strip -s ./elm-format


FROM scratch as artifact
COPY --from=build /elm-format/elm-format /elm-format

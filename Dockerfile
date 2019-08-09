# stack installation from https://github.com/samdoshi/docker-haskell-stack
FROM buildpack-deps:latest

ENV STACK_VERSION 2.1.3

ENV STACK_DOWNLOAD_URL https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz
ENV DEBIAN_FRONTEND noninteractive
ENV PATH $PATH:/root/.local/bin
ENV LANG C.UTF-8

RUN apt-get update -q && \
    apt-get install -qy libgmp-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /root/.local/bin && \
    wget -q -O- $STACK_DOWNLOAD_URL | tar --strip=1 -xvz -C /root/.local/bin/ && \
    chmod +x /root/.local/bin/stack


# Install elm-format dependencies
COPY stack.yaml ./
COPY elm-format.cabal ./
RUN stack setup
RUN stack install shake

RUN stack build --only-snapshot
RUN stack build --only-dependencies
RUN stack build --test --only-dependencies

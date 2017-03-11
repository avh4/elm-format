FROM fpco/stack-build:lts-7.4

COPY stack.yaml elm-format.cabal ./
RUN stack setup
RUN stack build --only-snapshot
RUN stack build --only-dependencies
RUN stack build --test --only-dependencies

COPY README.md LICENSE ./
COPY src/ ./src/
COPY parser/ ./parser/

RUN stack build

COPY tests/ ./tests/
RUN ./tests/run-tests.sh

# binaries are in .stack-work/install/x86_64-linux/lts-7.4/8.0.1/bin

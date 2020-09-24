#!/bin/bash

set -euo pipefail

for i in "$@"; do
    echo "Extracting $i"
    unzip -d dist/ "$i"
done


echo "Verifying signatures"
for i in dist/*.{zip,tgz}; do
    echo ">>> $i"
    if [ -f "$i.sig" ]; then
        gpgv --keyring "$(pwd)"/keys/github-actions.gpg "$i".sig "$i"
    else
        echo "WARNING: No signature"
    fi
done

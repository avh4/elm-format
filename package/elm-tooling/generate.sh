#!/bin/bash
set -euo pipefail

VERSION="$1"

function print-version() {
  PLATFORM="$1"
  PLATFORM2="$2"
  ARCH="$3"
  TYPE="$4"
  EXT="${5-}"

  FILENAME="elm-format-$VERSION-$PLATFORM2-$ARCH.$TYPE"

  echo "  $PLATFORM: {"
  echo -e "    hash:\n      \"$(sha256sum "../../publish/$VERSION/$FILENAME" | cut -d' ' -f1)\","
  echo -e "    url:\n      \"https://github.com/avh4/elm-format/releases/download/$VERSION/$FILENAME\","
  echo "    fileName: \"elm-format$EXT\","
  echo "    type: \"$TYPE\","
  echo "  },"
}

echo "\"$VERSION\": {"
print-version linux linux x64 tgz
print-version mac mac x64 tgz
print-version windows win x64 zip .exe
echo "},"

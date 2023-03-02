#!/bin/bash
set -euo pipefail

VERSION="$1"

function print-version() {
  PLATFORM_ELM_TOOLING="$1"
  PLATFORM_ELM_FORMAT="$2"
  ZIP_TYPE="$3"
  EXT="${4-}"

  FILENAME="elm-format-$VERSION-$PLATFORM_ELM_FORMAT.$ZIP_TYPE"
  FILEPATH="../../publish/$VERSION/$FILENAME"

  echo "  \"$PLATFORM_ELM_TOOLING\": {"
  echo "    hash: \"$(sha256sum "$FILEPATH" | cut -d' ' -f1)\","
  echo "    url: \"https://github.com/avh4/elm-format/releases/download/$VERSION/$FILENAME\","
  echo "    fileSize: $(stat -c %s "$FILEPATH"),"
  echo "    fileName: \"elm-format$EXT\","
  echo "    type: \"$ZIP_TYPE\","
  echo "  },"
}

echo "\"$VERSION\": {"
print-version darwin-arm64 mac-arm64 tgz
print-version darwin-x64 mac-x64 tgz
print-version linux-arm64 linux-aarch64 tgz
print-version linux-x64 linux-x64 tgz
print-version win32-x64 win-x64 zip .exe
echo "},"

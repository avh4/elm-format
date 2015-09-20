#!/bin/bash

ELM_FORMAT=".cabal-sandbox/bin/elm-format"
if which md5 > /dev/null; then
	MD5="md5"
else
	MD5="md5sum"
fi

function check() {
	INPUT="tests/test-files/good/$1"
	OUTPUT="formatted.elm"
	echo
	echo "## $1"
	"$ELM_FORMAT" "$INPUT" --output "$OUTPUT" || exit 1
	diff -u "$OUTPUT" "$INPUT" || exit 1
	echo -n "Checksum: "
	"$MD5" "$OUTPUT"
}

echo
echo
echo "# elm-format test suite"

check Simple.elm
check AllSyntax.elm

echo
echo "# GREAT SUCCESS!"

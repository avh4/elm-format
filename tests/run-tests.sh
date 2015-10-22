#!/bin/bash

ELM_FORMAT=".cabal-sandbox/bin/elm-format"
if which md5 > /dev/null; then
	MD5="md5"
else
	MD5="md5sum"
fi

function checkGood() {
	INPUT="tests/test-files/good/$1"
	OUTPUT="formatted.elm"
	echo
	echo "## good/$1"
	"$ELM_FORMAT" "$INPUT" --output "$OUTPUT" || exit 1
	diff -u "$OUTPUT" "$INPUT" || exit 1
	echo -n "Checksum: "
	"$MD5" "$OUTPUT"
}

function checkTransformation() {
	INPUT="tests/test-files/transform/$1"
	OUTPUT="formatted.elm"
	EXPECTED="tests/test-files/transform/${1%.*}.formatted.elm"
	echo
	echo "## transform/$1"
	"$ELM_FORMAT" "$INPUT" --output "$OUTPUT" || exit 1
	diff -u "$OUTPUT" "$EXPECTED" || exit 1
	echo -n "Checksum: "
	"$MD5" "$OUTPUT"
}

echo
echo
echo "# elm-format test suite"

checkGood Simple.elm
checkGood AllSyntax.elm
checkGood evancz/start-app/StartApp.elm

checkTransformation Examples.elm

echo
echo "# GREAT SUCCESS!"

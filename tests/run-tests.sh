#!/bin/bash

ELM_FORMAT="../bin/elm-format"
MD5="md5"

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
check Imports.elm

echo
echo "# GREAT SUCCESS!"

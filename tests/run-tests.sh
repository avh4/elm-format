#!/bin/bash

ELM_FORMAT="../bin/elm-format"

function check() {
	INPUT="tests/test-files/good/$1"
	OUTPUT="formatted.elm"
	echo
	echo "## $1"
	$ELM_FORMAT "$INPUT" --output "$OUTPUT" || exit 1
	diff -u "$OUTPUT" "$INPUT" || exit 1
}

echo
echo
echo "# elm-format test suite"

check Simple.elm
check Imports.elm

echo
echo "# GREAT SUCCESS!"

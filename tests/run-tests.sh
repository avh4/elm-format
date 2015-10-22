#!/bin/bash

ELM_FORMAT=".cabal-sandbox/bin/elm-format"
if which md5 > /dev/null; then
	MD5="md5"
else
	MD5="md5sum"
fi

function returnCodeShouldEqual() {
	[ "$?" -eq "$1" ] || exit 1
}

function shouldOutputTheSame() {
	diff <(echo "$1") <(echo "$2") || exit 1
}

function checkWaysToRun() {
	INPUT="tests/test-files/good/$1"
	OUTPUT="formatted.elm"

	echo
	echo "## elm-format --help"
	HELP=`"$ELM_FORMAT" --help 2>&1`
	returnCodeShouldEqual 0
	echo "OK"

	echo
	echo "## elm-format -h"
	SHORTHELP=`"$ELM_FORMAT" -h 2>&1`
	returnCodeShouldEqual 0
	shouldOutputTheSame "$HELP" "$SHORTHELP"
	echo "OK"

	echo
	echo "## elm-format"
	NOARGS=`"$ELM_FORMAT" 2>&1`
	returnCodeShouldEqual 1
	shouldOutputTheSame "$HELP" "$NOARGS"
	echo "OK"

	echo
  echo "## elm-format INPUT (answer = y)"
	echo "y" | "$ELM_FORMAT" "$INPUT" 1>/dev/null
	returnCodeShouldEqual 0
	echo "OK"

	echo
  echo "## elm-format INPUT (answer = n)"
	echo "n" | "$ELM_FORMAT" "$INPUT" 1>/dev/null
	returnCodeShouldEqual 0
	echo "OK"

	echo
	echo "## elm-format INPUT --output OUTPUT"
	"$ELM_FORMAT" "$INPUT" --output "$OUTPUT"
	returnCodeShouldEqual 0
	echo "OK"
}

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

checkWaysToRun Simple.elm

checkGood Simple.elm
checkGood AllSyntax.elm
checkGood evancz/start-app/StartApp.elm

checkTransformation Examples.elm

echo
echo "# GREAT SUCCESS!"

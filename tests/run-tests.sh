#!/bin/bash

cabal test || exit 1
cabal build || exit 1

ELM_FORMAT="./dist/build/elm-format/elm-format"
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

function compareFiles() {
	EXPECTED="$1"
	ACTUAL="$2"

	diff -u "$ACTUAL" "$EXPECTED"
	returnCodeShouldEqual 0
	echo -n "Checksum: "
	"$MD5" "$ACTUAL"
}

function checkWaysToRun() {
	cp "tests/test-files/good/$1" "_input.elm"
	INPUT="_input.elm"
	OUTPUT="formatted.elm"
	DIRECTORY="tests/test-files/directory"
	RECURSIVE_DIRECTORY="tests/test-files/recursive-directory"

	NONEXISTENT="DoesNotExist.elm"
	EMPTY_DIR=$(mktemp -d -t elm-format-tests.XXXXXXXXXX)

	echo
	echo "------------------------------"
	echo "# WAYS TO RUN"
	echo

	echo "## elm-format --help"
	HELP=$("$ELM_FORMAT" --help 2>&1)
	returnCodeShouldEqual 0

	echo "## elm-format -h"
	SHORTHELP=$("$ELM_FORMAT" -h 2>&1)
	returnCodeShouldEqual 0
	shouldOutputTheSame "$HELP" "$SHORTHELP"

	echo "## elm-format"
	NOARGS=$("$ELM_FORMAT" 2>&1)
	returnCodeShouldEqual 0
	shouldOutputTheSame "$HELP" "$NOARGS"

	echo "## elm-format INPUT (answer = y)"
	echo "y" | "$ELM_FORMAT" "$INPUT" 1>/dev/null
	returnCodeShouldEqual 0

	echo "## elm-format INPUT (answer = n)"
	echo "n" | "$ELM_FORMAT" "$INPUT" 1>/dev/null
	returnCodeShouldEqual 0

	echo "## elm-format INPUT --yes"
	"$ELM_FORMAT" "$INPUT" --yes 1>/dev/null
	returnCodeShouldEqual 0

	echo "## elm-format --yes INPUT"
	"$ELM_FORMAT" --yes "$INPUT" 1>/dev/null
	returnCodeShouldEqual 0

	echo "## elm-format NONEXISTENT"
	"$ELM_FORMAT" "$NONEXISTENT" 1>/dev/null
	returnCodeShouldEqual 1

	echo "## elm-format INPUT --output OUTPUT"
	"$ELM_FORMAT" "$INPUT" --output "$OUTPUT" 1>/dev/null
	returnCodeShouldEqual 0
	compareFiles "$INPUT" "$OUTPUT" 1>/dev/null

	echo "## elm-format DIRECTORY --output OUTPUT"
	"$ELM_FORMAT" "$DIRECTORY" --output "$OUTPUT" 1>/dev/null
	returnCodeShouldEqual 1

	echo "## elm-format DIRECTORY (answer = n)"
	echo "n" | "$ELM_FORMAT" "$DIRECTORY" 1>/dev/null
	returnCodeShouldEqual 0

	echo "## elm-format DIRECTORY (answer = y)"
	echo "y" | "$ELM_FORMAT" "$RECURSIVE_DIRECTORY" 1>/dev/null 2>/dev/null
	returnCodeShouldEqual 1
	# invalid file in the nested directory
	# if recursion didn't work, return code would be 0
	# because it never got to the nested invalid file

	echo "## elm-format DIRECTORY --yes"
	"$ELM_FORMAT" "$DIRECTORY" --yes 1>/dev/null 2>/dev/null
	returnCodeShouldEqual 1

	echo "## elm-format EMPTY_DIRECTORY"
	"$ELM_FORMAT" "$EMPTY_DIR" 1>/dev/null
	returnCodeShouldEqual 1

	echo "# OK!"
	echo "------------------------------"
}

function checkGood() {
	INPUT="tests/test-files/good/$1"
	OUTPUT="formatted.elm"

	echo
	echo "## good/$1"
	time "$ELM_FORMAT" "$INPUT" --output "$OUTPUT" 1>/dev/null
	returnCodeShouldEqual 0
	compareFiles "$INPUT" "$OUTPUT"
}

function checkTransformation() {
	INPUT="tests/test-files/transform/$1"
	OUTPUT="formatted.elm"
	EXPECTED="tests/test-files/transform/${1%.*}.formatted.elm"

	echo
	echo "## transform/$1"
	time "$ELM_FORMAT" "$INPUT" --output "$OUTPUT" 1>/dev/null
	returnCodeShouldEqual 0
	compareFiles "$EXPECTED" "$OUTPUT"
}


echo
echo
echo "# elm-format test suite"

checkWaysToRun Simple.elm

checkGood Simple.elm
checkGood AllSyntax.elm
checkGood Comments.elm
checkGood evancz/start-app/StartApp.elm
checkGood TheSeamau5/elm-check/Check.elm
checkGood rtfeldman/dreamwriter/Editor.elm
checkGood rtfeldman/dreamwriter/LeftSidebar.elm
checkGood rtfeldman/dreamwriter/RightSidebar.elm
checkGood rtfeldman/dreamwriter/WordGraph.elm
checkGood avh4/elm-fifo/Fifo.elm

checkTransformation Examples.elm

echo
echo "# GREAT SUCCESS!"

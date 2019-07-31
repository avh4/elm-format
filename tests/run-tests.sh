#!/usr/bin/env bash
# shellcheck disable=SC2002

# TODO: convert the rest of these tests to haskell in CliTest.hs

uname -s
command -v bash
command -v diff
command -v grep
command -v sed
command -v tee


ELM_FORMAT="$1"
if [ ! -e "$ELM_FORMAT" ]; then
	echo "$0: ERROR: $ELM_FORMAT not found" >&2
	exit 1
fi

if command -v md5 > /dev/null; then
	MD5="md5"
else
	MD5="md5sum"
fi

function returnCodeShouldEqual() {
	[ "$?" -eq "$1" ] || exit 1
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
	cp "tests/test-files/good/Elm-0.18/Simple.elm" "_input.elm"
	cp "tests/test-files/transform/Elm-0.18/Examples.elm" "_input2.elm"

	INPUT="_input.elm"
	INPUT_2="_input2.elm"
	OUTPUT="formatted.elm"
	DIRECTORY="tests/test-files/directory"
	RECURSIVE_DIRECTORY="tests/test-files/recursive-directory"

	NONEXISTENT="DoesNotExist.elm"
	EMPTY_DIR=$(mktemp -d -t elm-format-tests.XXXXXXXXXX)

	echo
	echo "------------------------------"
	echo "# WAYS TO RUN"
	echo

	echo "## elm-format INPUT (answer = y)"
	echo "y" | "$ELM_FORMAT" "$INPUT" --elm-version=0.19 1>/dev/null
	returnCodeShouldEqual 0

	echo "## elm-format INPUT (answer = n)"
	echo "n" | "$ELM_FORMAT" "$INPUT" --elm-version=0.19 1>/dev/null
	returnCodeShouldEqual 0

	echo "## elm-format INPUT --yes"
	"$ELM_FORMAT" "$INPUT" --elm-version=0.19 --yes 1>/dev/null
	returnCodeShouldEqual 0

	echo "## elm-format --yes INPUT"
	"$ELM_FORMAT" --elm-version=0.19 --yes "$INPUT" 1>/dev/null
	returnCodeShouldEqual 0

	echo "## elm-format NONEXISTENT"
	"$ELM_FORMAT" --elm-version=0.19 "$NONEXISTENT" 1>/dev/null
	returnCodeShouldEqual 1

	echo "## elm-format INPUT --output OUTPUT"
	"$ELM_FORMAT" "$INPUT" --elm-version=0.19 --output "$OUTPUT" 1>/dev/null
	returnCodeShouldEqual 0
	compareFiles "$INPUT" "$OUTPUT" 1>/dev/null

	echo "## cat INPUT | elm-format --stdin"
	cat "$INPUT" | "$ELM_FORMAT" --elm-version=0.19 --stdin > "$OUTPUT"
	returnCodeShouldEqual 0
	compareFiles "$INPUT" "$OUTPUT"

	echo "## cat INPUT | elm-format --stdin INPUT"
	cat "$INPUT" | "$ELM_FORMAT" --elm-version=0.19 --stdin "$INPUT"
	returnCodeShouldEqual 1

	echo "## cat INPUT | elm-format --stdin --output OUTPUT"
	cat "$INPUT" | "$ELM_FORMAT" --elm-version=0.19 --stdin --output "$OUTPUT" 1>/dev/null
	returnCodeShouldEqual 0
	compareFiles "$INPUT" "$OUTPUT" 1>/dev/null

	echo "## cat INPUT | elm-format INPUT --stdin --output OUTPUT"
	cat "$INPUT" | "$ELM_FORMAT" "$INPUT" --elm-version=0.19 --stdin --output "$OUTPUT" 1>/dev/null
	returnCodeShouldEqual 1

	echo "## elm-format DIRECTORY --output OUTPUT"
	"$ELM_FORMAT" "$DIRECTORY" --elm-version=0.19 --output "$OUTPUT" 1>/dev/null
	returnCodeShouldEqual 1

	echo "## elm-format DIRECTORY (answer = n)"
	echo "n" | "$ELM_FORMAT" --elm-version=0.19 "$DIRECTORY" 1>/dev/null
	returnCodeShouldEqual 0

	echo "## elm-format DIRECTORY (answer = y)"
	echo "y" | "$ELM_FORMAT" --elm-version=0.19 "$RECURSIVE_DIRECTORY" 1>/dev/null 2>/dev/null
	returnCodeShouldEqual 1
	# invalid file in the nested directory
	# if recursion didn't work, return code would be 0
	# because it never got to the nested invalid file

	echo "## elm-format DIRECTORY --yes"
	"$ELM_FORMAT" "$DIRECTORY" --elm-version=0.19 --yes 1>/dev/null 2>/dev/null
	returnCodeShouldEqual 1

	echo "## elm-format EMPTY_DIRECTORY"
	"$ELM_FORMAT" "$EMPTY_DIR" --elm-version=0.19 1>/dev/null
	returnCodeShouldEqual 1

	echo "## elm-format INPUT --validate"
	"$ELM_FORMAT" "$INPUT" --elm-version=0.19 --validate 1>/dev/null
	returnCodeShouldEqual 0

	echo "## elm-format INPUT --validate --yes"
	"$ELM_FORMAT" "$INPUT" --elm-version=0.19 --validate --yes 1>/dev/null
	returnCodeShouldEqual 0

	echo "# OK!"
	echo "------------------------------"
}

function checkValidationOutputFormat() {
	if uname -s | grep -q 'MSYS\|MINGW'; then
		# TODO: debug why checkValidationOutputFormat doesn't work on appveyor, and/or rewrite these tests in haskell
		echo "Running on Windows; skipping checkValidationOutputFormat"
		return
	fi
	cp "tests/test-files/transform/Elm-0.18/Examples.elm" "_input.elm"
	cp "tests/test-files/transform/Elm-0.18/Examples.elm" "_input2.elm"

	INPUT="_input.elm"
	INPUT_2="_input2.elm"
	OUTPUT="_stdout.txt"

	echo
	echo "------------------------------"
	echo "# VALIDATION OUTPUT IN JSON"
	echo

	echo "## with unformatted files outputs in expected json format"
	"$ELM_FORMAT" "$INPUT" "$INPUT_2" --elm-version=0.19 --validate | sed -e "s/ elm-format-[-.0-9a-z]* / elm-format-<version> /" | tee "$OUTPUT"
	compareFiles tests/test-files/validate1.json "$OUTPUT"

	echo "## with formatted file with output in json outputs empty list"
	"$ELM_FORMAT" "$INPUT" "$INPUT_2" --elm-version=0.19 --yes > /dev/null
	"$ELM_FORMAT" "$INPUT" "$INPUT_2" --elm-version=0.19 --validate | tee "$OUTPUT"
	compareFiles tests/test-files/validate2.json "$OUTPUT"

	echo "# OK!"
	echo "------------------------------"
}

echo
echo
echo "# elm-format test suite"

checkWaysToRun
checkValidationOutputFormat

echo
echo "# GREAT SUCCESS!"

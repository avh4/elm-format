#!/usr/bin/env bash
# shellcheck disable=SC2002

uname -s
command -v bash
command -v diff
command -v grep
command -v sed
command -v tee
command -v wc
command -v stack


# if command -v nix-env; then
# 	echo "$0: INFO: Detected Nixos or Nix"
# 	STACK_ARGS=(--nix-pure --nix-add-gc-roots)
# 	echo "$0: INFO: nix arguments will be passed to stack: ${STACK_ARGS[*]}"
# fi


ELM_FORMAT="$(stack "${STACK_ARGS[@]}" path --local-install-root)/bin/elm-format"
if [ ! -e "$ELM_FORMAT" ]; then
	echo "$0: ERROR: $ELM_FORMAT not found" >&2
	exit 1
fi
echo "Testing $ELM_FORMAT"
cp "$ELM_FORMAT" tests/elm-format
ELM_FORMAT="tests/elm-format"

if command -v md5 > /dev/null; then
	MD5="md5"
else
	MD5="md5sum"
fi

function returnCodeShouldEqual() {
	[ "$?" -eq "$1" ] || exit 1
}

function shouldOutputTheSameIgnoringEol() {
	diff -u --ignore-space-change <("${1//.exe/}") <("${2//.exe/}") || exit 1
}

function outputShouldRoughlyMatchPatterns() {
	PATTERNS_FILE="$1"
	echo "DEBUG: PATTERNS_FILE=$PATTERNS_FILE"
	OUTPUT="$2"
	echo "DEBUG: OUTPUT=$OUTPUT"

	MATCHES=$(echo "$OUTPUT" | grep -F -f "$PATTERNS_FILE")
	echo "DEBUG: MATCHES=$MATCHES"
	echo "DEBUG: $(echo "$MATCHES" | wc -l)"
	[[ "$(echo "$MATCHES" | wc -l)" == "$(wc -l < "$PATTERNS_FILE")" ]] || exit 1
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
	cp "tests/test-files/good/Simple.elm" "_input.elm"
	cp "tests/test-files/transform/Examples.elm" "_input2.elm"

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

	HELP=$(cat tests/usage.stdout)

	echo "## elm-format --help"
	LONGHELP=$("$ELM_FORMAT" --help 2>&1)
	returnCodeShouldEqual 0
	shouldOutputTheSameIgnoringEol "$HELP" "$LONGHELP"

	echo "## elm-format -h"
	SHORTHELP=$("$ELM_FORMAT" -h 2>&1)
	returnCodeShouldEqual 0
	shouldOutputTheSameIgnoringEol "$HELP" "$SHORTHELP"

	echo "## elm-format"
	NOARGS=$("$ELM_FORMAT" 2>&1)
	returnCodeShouldEqual 0
	shouldOutputTheSameIgnoringEol "$HELP" "$NOARGS"

	echo "## elm-format INPUT --validate does not change things"
	"$ELM_FORMAT" "$INPUT_2" --elm-version=0.19 --validate 1>/dev/null
	compareFiles "tests/test-files/transform/Examples.elm" "$INPUT_2"
	returnCodeShouldEqual 0

	echo "## elm-format INPUT --validate with unformatted file exits 1"
	"$ELM_FORMAT" "$INPUT_2" --elm-version=0.19 --validate 1>/dev/null
	returnCodeShouldEqual 1

	echo "## elm-format INPUT --validate with formatted file exits 0"
	"$ELM_FORMAT" "$INPUT_2" --elm-version=0.19 --yes 1>/dev/null
	"$ELM_FORMAT" "$INPUT_2" --elm-version=0.19 --validate 1>/dev/null
	returnCodeShouldEqual 0

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
	STDOUT=$(cat "$INPUT" | "$ELM_FORMAT" --elm-version=0.19 --stdin "$INPUT" 2>&1)
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

function checkGood() {
	ELM_VERSION="$1"
	INPUT="tests/test-files/good/$2"
	OUTPUT="formatted.elm"

	if [ ! -e "$INPUT" ]; then
		echo "ERROR: $INPUT does not exist"
		exit 1
	fi

	echo
	echo "## good/$2"
	time "$ELM_FORMAT" "$INPUT" --output "$OUTPUT" --elm-version "$1" 1>/dev/null
	returnCodeShouldEqual 0
	compareFiles "$INPUT" "$OUTPUT"
}

function checkGoodAllSyntax() {
	checkGood "$1" "AllSyntax/$1/$2.elm"
	checkGood "$1" "AllSyntax/$1/BlockComments/$2.elm"
	checkGood "$1" "AllSyntax/$1/LineComments/$2.elm"
}

function checkBad() {
	INPUT="tests/test-files/bad/$1"
	EXPECTED="tests/test-files/bad/${1%.*}.output.txt"

	echo "## bad/$1"
	if uname -s | grep -q 'MSYS\|MINGW'; then
		# TODO: debug why checkBad doesn't work on appveyor, and/or rewrite these tests in haskell
		echo "Running on Windows; skipping checkBad"
		return
	fi
	STDOUT=$(cat "$INPUT" | "$ELM_FORMAT" --elm-version=0.19 --stdin 2>&1)
	returnCodeShouldEqual 1
	echo "DEBUG: return code was 1"
	outputShouldRoughlyMatchPatterns "$EXPECTED" "$STDOUT"
}

function checkTransformation() {
	ELM_VERSION="$1"
	INPUT="tests/test-files/transform/$2"
	OUTPUT="formatted.elm"
	EXPECTED="tests/test-files/transform/${2%.*}.formatted.elm"

	echo
	echo "## transform/$2"
	time "$ELM_FORMAT" "$INPUT" --output "$OUTPUT" --elm-version "$ELM_VERSION" 1>/dev/null
	returnCodeShouldEqual 0
	compareFiles "$EXPECTED" "$OUTPUT"
}

function checkUpgrade() {
	ELM_VERSION="$1"
	INPUT="tests/test-files/transform/$2"
	OUTPUT="formatted.elm"
	EXPECTED="tests/test-files/transform/${2%.*}.formatted.elm"

	echo
	echo "## transform/$2"
	time "$ELM_FORMAT" "$INPUT" --output "$OUTPUT" --upgrade --elm-version "$ELM_VERSION" 1>/dev/null
	returnCodeShouldEqual 0
	compareFiles "$EXPECTED" "$OUTPUT"
}

function checkValidationOutputFormat() {
	if uname -s | grep -q 'MSYS\|MINGW'; then
		# TODO: debug why checkValidationOutputFormat doesn't work on appveyor, and/or rewrite these tests in haskell
		echo "Running on Windows; skipping checkValidationOutputFormat"
		return
	fi
	cp "tests/test-files/transform/Examples.elm" "_input.elm"
	cp "tests/test-files/transform/Examples.elm" "_input2.elm"

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

checkGood 0.19 AllSyntax/0.19/Literals.elm
checkGood 0.19 AllSyntax/0.19/InfixDeclaration.elm
checkGood 0.19 AllSyntax/0.19/Patterns.elm

checkTransformation 0.19 ClarifyNegativeExponentiationPrecedence.elm
checkTransformation 0.19 Elm-0.19/ExplicitlyListExposedValues.elm
checkTransformation 0.19 Elm-0.19/ParseTabsInLiterals.elm
checkTransformation 0.19 Elm-0.19/DistinguishCharStartFromRef.elm

checkUpgrade 0.19 Elm-0.19/CommaFunctionsBecomeLambdas.elm
checkUpgrade 0.19 Elm-0.19/RemoveBangOperator.elm
checkUpgrade 0.19 Elm-0.19/NewStringEscapeSyntax.elm
checkUpgrade 0.19 Elm-0.19/RemoveBasicsFlip.elm
checkUpgrade 0.19 Elm-0.19/RemoveBasicsCurry.elm
checkUpgrade 0.19 Elm-0.19/RemoveBasicsUncurry.elm
checkUpgrade 0.19 Elm-0.19/ConvertBasicsRem.elm
checkUpgrade 0.19 Elm-0.19/ConvertBasicsModBy.elm
checkUpgrade 0.19 Elm-0.19/ConvertHtmlAttributesStyle.elm
checkUpgrade 0.19 Elm-0.19/ConvertHtmlAttributesStyleAltImport.elm
checkUpgrade 0.19 Elm-0.19/ListExports.elm
checkUpgrade 0.19 Elm-0.19/OpenExplicitConstructorImports.elm

checkGood 0.18 Simple.elm
checkGood 0.18 AllSyntax/0.18/AllSyntax.elm
checkGoodAllSyntax 0.18 Module
checkGood 0.18 AllSyntax/0.18/ModuleMultiline.elm
checkGood 0.18 AllSyntax/0.18/ModuleWithDocs.elm
checkGood 0.18 AllSyntax/0.18/Declarations.elm
checkGoodAllSyntax 0.18 Patterns
checkGoodAllSyntax 0.18 Types
checkGood 0.18 AllSyntax/0.18/OldKeywords.elm
checkGood 0.18 AllSyntax/0.18/Expressions.elm
checkGood 0.18 AllSyntax/0.18/Expressions/Unary.elm
checkGood 0.18 AllSyntax/0.18/Expressions/BinaryOperators.elm
checkGood 0.18 AllSyntax/0.18/DocComments.elm
checkGood 0.18 AllSyntax/0.18/DocCommentsStartingWtihAtDocs.elm
checkGood 0.18 AllSyntax/0.18/DeclaraionSpacing.elm
checkGood 0.18 Comments.elm
checkGood 0.18 AllSyntax/0.18/GLShader.elm
checkGood 0.18 AllSyntax/0.18/Literals.elm
checkGood 0.18 AllSyntax/0.18/Comments.elm
checkGood 0.18 ApiSketch.elm
checkTransformation 0.18 AllSyntax/0.18/Types.elm
checkTransformation 0.18 AllSyntax/0.18/Patterns.elm

# checkGood 0.16 AllSyntax/0.16/PatternsRequireParens.elm

checkGood 0.17 AllSyntax/0.17/Range.elm
checkGood 0.17 AllSyntax/0.17/InfixOperators.elm
checkGood 0.17 AllSyntax/0.17/OldKeywords.elm
checkGood 0.17 Export.elm
checkGoodAllSyntax 0.17 Module
checkGoodAllSyntax 0.17 ModuleEffect

checkGood 0.18 Export.elm
checkGood 0.18 TrueFalseInIdentifiers.elm
checkGood 0.18 TopLevelSpacing.elm
checkGood 0.18 WorkaroundNegativeCasePatterns.elm

checkGood 0.18 evancz/start-app/StartApp.elm
checkGood 0.18 TheSeamau5/elm-check/Check.elm
checkGood 0.18 rtfeldman/dreamwriter/Editor.elm
checkGood 0.18 rtfeldman/dreamwriter/LeftSidebar.elm
checkGood 0.18 rtfeldman/dreamwriter/RightSidebar.elm
checkGood 0.18 rtfeldman/dreamwriter/WordGraph.elm
checkGood 0.18 avh4/elm-fifo/Fifo.elm
checkGood 0.19 mdgriffith/style-elements/Element.elm
checkGood 0.19 elm/core/Basics.elm

checkGood 0.17 elm-lang/examples/random.elm
checkGood 0.17 elm-lang/examples/http.elm
checkGood 0.17 elm-lang/examples/time.elm
checkGood 0.17 elm-lang/examples/websockets.elm
checkGood 0.17 elm-lang/examples/Spelling.elm
checkGood 0.17 elm-lang/websocket/WebSocket.elm

checkBad UnexpectedComma.elm
checkBad UnexpectedEndOfInput.elm

checkTransformation 0.18 Examples.elm
checkTransformation 0.18 TrickyModule1.elm
checkTransformation 0.18 TrickyModule2.elm
checkTransformation 0.18 TrickyModule3.elm
checkTransformation 0.18 TrickyModule4.elm
checkTransformation 0.18 LenientEqualsColon.elm
# checkTransformation 0.16 Elm-0.16/github-avh4-elm-format-184.elm
checkTransformation 0.18 QuickCheck-4562ebccb71ea9f622fb99cdf32b2923f6f9d34f-2529668492575674138.elm
checkTransformation 0.18 QuickCheck-94f37da84c1310f03dcfa1059ce870b73c94a825--6449652945938213463.elm
checkTransformation 0.18 WindowsEol.elm
checkTransformation 0.18 DocCommentCodeExample.elm
checkTransformation 0.18 DocCommentMarkdownSafety.elm
checkTransformation 0.18 DocCommentCheapskateReferenceBug.elm
checkTransformation 0.18 DocCommentAtDocs.elm
checkTransformation 0.18 Sorting.elm
checkTransformation 0.18 UnnecessaryParens.elm
checkTransformation 0.18 Empty.elm
checkTransformation 0.18 MissingImportParens.elm
checkUpgrade 0.18 Elm-0.18/PrimesBecomeUnderscores.elm
checkUpgrade 0.18 Elm-0.18/RangesBecomeListRange.elm
checkUpgrade 0.18 Elm-0.18/BackticksBecomeFunctionCalls.elm
checkUpgrade 0.18 Elm-0.18/SpecialBackticksBecomePipelines.elm
checkUpgrade 0.18 Elm-0.18/RenameTupleFunctions.elm

checkValidationOutputFormat

echo
echo "# GREAT SUCCESS!"

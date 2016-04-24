#!/usr/bin/env shjs

require('shelljs/global');

function execSuccess(cmd) {
	var result = exec(cmd);
	if (result.code !== 0) {
		exit(1);
	}
}

function execSilentSuccess(cmd) {
	var result = exec(cmd, {silent:true});
	if (result.code !== 0) {
		exit(1);
	}
}

function captureStderr(cmd) {
	var result = exec(cmd, {silent:true});
	if (result.code !== 0) exit(1);
	return result.stderr;
}

execSuccess('shellcheck "./build-package.sh"');
execSuccess('shellcheck "./package/linux/build-package.sh"');

execSuccess('cabal test');
execSuccess('cabal build');


if (which('md5')) {
	MD5="md5"
} else {
	MD5="md5sum"
}

// # function returnCodeShouldEqual() {
// # 	[ "$?" -eq "$1" ] || exit 1
// # }

// function shouldOutputTheSame() {
// 	diff <(echo "$1") <(echo "$2") || exit 1
// }

// function outputShouldRoughlyMatchPatterns() {
// 	PATTERNS_FILE="$1"
// 	OUTPUT="$2"
//
// 	MATCHES=$(echo "$OUTPUT" | grep -F -f "$PATTERNS_FILE")
// 	[[ "$(echo "$MATCHES" | wc -l)" == "$(wc -l < "$PATTERNS_FILE")" ]] || exit 1
// }

function checkWaysToRun(file) {
	cp("tests/test-files/good/" + file, "_input.elm");
	INPUT="_input.elm"
	OUTPUT="formatted.elm"
	DIRECTORY="tests/test-files/directory"
	RECURSIVE_DIRECTORY="tests/test-files/recursive-directory"

	NONEXISTENT="DoesNotExist.elm"
	EMPTY_DIR=exec("mktemp -d -t elm-format-tests.XXXXXXXXXX").stdout;

	echo();
	echo("------------------------------");
	echo("# WAYS TO RUN");
	echo();

	var result;

	echo("## elm-format INPUT (answer = y)");
	execSilentSuccess('echo "y" | ' + ELM_FORMAT + ' ' + INPUT);

	echo("## elm-format INPUT (answer = n)");
	execSilentSuccess('echo "n" | ' + ELM_FORMAT + ' ' + INPUT);

// 	echo "## elm-format INPUT --yes"
// 	"$ELM_FORMAT" "$INPUT" --yes 1>/dev/null
// 	returnCodeShouldEqual 0
//
// 	echo "## elm-format --yes INPUT"
// 	"$ELM_FORMAT" --yes "$INPUT" 1>/dev/null
// 	returnCodeShouldEqual 0
//
// 	echo "## elm-format NONEXISTENT"
// 	"$ELM_FORMAT" "$NONEXISTENT" 1>/dev/null
// 	returnCodeShouldEqual 1
//
// 	echo "## elm-format INPUT --output OUTPUT"
// 	"$ELM_FORMAT" "$INPUT" --output "$OUTPUT" 1>/dev/null
// 	returnCodeShouldEqual 0
// 	compareFiles "$INPUT" "$OUTPUT" 1>/dev/null
//
// 	echo "## cat INPUT | elm-format --stdin"
// 	cat "$INPUT" | "$ELM_FORMAT" --stdin > "$OUTPUT"
// 	returnCodeShouldEqual 0
// 	compareFiles "$INPUT" "$OUTPUT"
//
// 	echo "## cat INPUT | elm-format --stdin INPUT"
// 	STDOUT=$(cat "$INPUT" | "$ELM_FORMAT" --stdin "$INPUT" 2>&1)
// 	returnCodeShouldEqual 1
//
// 	echo "## cat INPUT | elm-format --stdin --output OUTPUT"
// 	cat "$INPUT" | "$ELM_FORMAT" --stdin --output "$OUTPUT" 1>/dev/null
// 	returnCodeShouldEqual 0
// 	compareFiles "$INPUT" "$OUTPUT" 1>/dev/null
//
// 	echo "## cat INPUT | elm-format INPUT --stdin --output OUTPUT"
// 	cat "$INPUT" | "$ELM_FORMAT" "$INPUT" --stdin --output "$OUTPUT" 1>/dev/null
// 	returnCodeShouldEqual 1
//
// 	echo "## elm-format DIRECTORY --output OUTPUT"
// 	"$ELM_FORMAT" "$DIRECTORY" --output "$OUTPUT" 1>/dev/null
// 	returnCodeShouldEqual 1
//
// 	echo "## elm-format DIRECTORY (answer = n)"
// 	echo "n" | "$ELM_FORMAT" "$DIRECTORY" 1>/dev/null
// 	returnCodeShouldEqual 0
//
// 	echo "## elm-format DIRECTORY (answer = y)"
// 	echo "y" | "$ELM_FORMAT" "$RECURSIVE_DIRECTORY" 1>/dev/null 2>/dev/null
// 	returnCodeShouldEqual 1
// 	# invalid file in the nested directory
// 	# if recursion didn't work, return code would be 0
// 	# because it never got to the nested invalid file
//
// 	echo "## elm-format DIRECTORY --yes"
// 	"$ELM_FORMAT" "$DIRECTORY" --yes 1>/dev/null 2>/dev/null
// 	returnCodeShouldEqual 1
//
// 	echo "## elm-format EMPTY_DIRECTORY"
// 	"$ELM_FORMAT" "$EMPTY_DIR" 1>/dev/null
// 	returnCodeShouldEqual 1
}


checkWaysToRun("Simple.elm");

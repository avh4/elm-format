#!/bin/bash

ELM_FORMAT="$1"
INPUT="$2"
OUTPUT="$3"

time cat "$INPUT" | "$ELM_FORMAT" --elm-version=0.19 --stdin --json | python -mjson.tool | tr -d '\015' > "$OUTPUT"

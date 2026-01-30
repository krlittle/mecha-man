#!/usr/bin/env bash
#
# Convert a PNG spritesheet to NES CHR format.
# Usage: ./tools/convert.sh filename.png
#
# Looks for the file in assets/raw/ and outputs to assets/chr/
# with a .chr extension.

set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <filename.png>"
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

INPUT="$PROJECT_DIR/assets/raw/$1"
OUTPUT="$PROJECT_DIR/assets/chr/${1%.*}.chr"

if [ ! -f "$INPUT" ]; then
    echo "Error: File not found: $INPUT"
    exit 1
fi

python3 "$SCRIPT_DIR/png2chr.py" "$INPUT" "$OUTPUT"

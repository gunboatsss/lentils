#!/bin/sh
# Validate a Kconfig file using Lentils.Kconfig module
# Usage: ./scripts/validate-kconfig.sh [file]
# Default: lentils.Kconfig

KCONFIG="${1:-lentils.Kconfig}"
cd "$(dirname "$0")/.."

[ -f "$KCONFIG" ] || { echo "Error: $KCONFIG not found" >&2; exit 1; }

SCRIPT=$(mktemp /tmp/kconfig-validate-XXXX.lean)
trap 'rm -f "$SCRIPT"' EXIT

# Awk escapes the file content into a Lean string.
# Every char is either printed literally (if safe) or hex-escaped.
{
    echo 'import Lentils.Kconfig'
    echo 'open Lentils.Kconfig'
    printf 'def src : String := "'
    awk '
    {
        gsub(/\\/, "\\\\")
        gsub(/"/, "\\\"")
        printf "%s\\n", $0
    }
    ' "$KCONFIG"
    echo '"'
    cat << 'EOF'
#eval
  let errors := validate src
  match errors with
  | [] => IO.println "Kconfig is valid."
  | es => IO.eprintln $ "Validation errors:\n" ++ formatErrors es
EOF
} > "$SCRIPT"

lake env lean "$SCRIPT" 2>&1 || exit 1
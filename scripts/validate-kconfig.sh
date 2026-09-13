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

# Freshness: the committed Generated.lean must match .config, so that
# editing .config then running `lake build` can never silently use a
# stale committed config. Regenerate to a temp file and compare.
if [ -f .config ] && [ -f Lentils/Config/Generated.lean ]; then
  FRESH_TMP=$(mktemp /tmp/kconfig-fresh-XXXX.lean)
  trap 'rm -f "$SCRIPT" "$FRESH_TMP"' EXIT
  scripts/gen-config.sh .config "$FRESH_TMP" >/dev/null
  if ! cmp -s "$FRESH_TMP" Lentils/Config/Generated.lean; then
    echo "Error: Lentils/Config/Generated.lean is stale relative to .config." >&2
    echo "Run: scripts/gen-config.sh .config Lentils/Config/Generated.lean" >&2
    exit 1
  fi
  echo "Generated config is fresh."
fi
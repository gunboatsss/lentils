#!/bin/sh
# install-symlinks.sh — Install lentils applet wrapper scripts.
#
# Usage: ./scripts/install-symlinks.sh [BIN_DIR] [INSTALL_DIR]
#
# Creates tiny wrapper scripts that invoke `lentils <applet> "$@"`.
# (Plain symlinks don't work because Lean 4's `main` doesn't receive argv[0].)
#
# BIN_DIR:      directory containing the lentils binary
#               (default: .lake/build/bin)
# INSTALL_DIR:  target directory for wrappers
#               (default: ./.lentils-bin — local only, never pollutes PATH)

set -e

# Safety gate: prevent accidental system pollution with work-in-progress wrappers.
# Agents and scripts must explicitly opt in by setting this env var.
if [ "${LENTILS_INSTALL_SAFETY:-}" != "1" ]; then
    echo "ERROR: This script installs lentils applet wrappers into your PATH." >&2
    echo "       Set LENTILS_INSTALL_SAFETY=1 to confirm you want to do this." >&2
    exit 1
fi

BIN_DIR="${1:-$PWD/.lake/build/bin}"
INSTALL_DIR="${2:-$PWD/.lentils-bin}"
COREUTILS="$(cd "$BIN_DIR" && pwd)/lentils"

mkdir -p "$INSTALL_DIR"

for app in cat echo true false pwd yes sleep basename dirname head tail wc uniq tee printf cut tr sort test grep; do
    cat > "$INSTALL_DIR/$app" <<EOF
#!/bin/sh
exec "$COREUTILS" "$app" "\$@"
EOF
    chmod +x "$INSTALL_DIR/$app"
done

echo "Installed lentils applet wrappers to $INSTALL_DIR"

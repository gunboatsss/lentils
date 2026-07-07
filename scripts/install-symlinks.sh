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
#               (default: $HOME/.local/bin)

set -e

BIN_DIR="${1:-$PWD/.lake/build/bin}"
INSTALL_DIR="${2:-$HOME/.local/bin}"
COREUTILS="$(cd "$BIN_DIR" && pwd)/lentils"

mkdir -p "$INSTALL_DIR"

for app in cat echo true false pwd yes sleep basename dirname head tail wc uniq tee printf; do
    cat > "$INSTALL_DIR/$app" <<EOF
#!/bin/sh
exec "$COREUTILS" "$app" "\$@"
EOF
    chmod +x "$INSTALL_DIR/$app"
done

echo "Installed lentils applet wrappers to $INSTALL_DIR"

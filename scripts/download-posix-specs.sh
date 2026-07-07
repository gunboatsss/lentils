#!/bin/bash
# download-posix-specs.sh — Download POSIX.1-2017 utility spec pages for local reference.
# Downloads HTML from pubs.opengroup.org and converts to markdown in wiki/posix/.
#
# NOTE: These files are © IEEE and The Open Group. All Rights Reserved.
# They are for DEVELOPMENT REFERENCE ONLY and are NOT redistributed.
# wiki/posix/ is gitignored.

set -e

BASE="https://pubs.opengroup.org/onlinepubs/9699919799/utilities"
DIR="$(cd "$(dirname "$0")/.." && pwd)/wiki/posix"

mkdir -p "$DIR"

UTILS=(
  cat echo true false pwd sort uniq head tail wc cut tr
  rm mv cp mkdir rmdir ln ls
)

for util in "${UTILS[@]}"; do
  echo "Downloading $util..."
  curl -sL "$BASE/$util.html" -o "$DIR/$util.html"
done

echo ""
echo "Downloaded ${#UTILS[@]} spec pages to $DIR/"
echo "These are © IEEE and The Open Group. All Rights Reserved."
echo "For development reference only — not redistributed."

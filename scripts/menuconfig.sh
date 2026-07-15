#!/bin/bash
# Minimal configuration TUI — pure bash, zero deps
# j/k/arrows: up/down  space: toggle  enter: save  q: quit

CONFIG="${1:-.config}"
cd "$(dirname "$0")/.."

[ -s "$CONFIG" ] || awk '/^config/{n=$2}/^\tbool"/&&n{print "CONFIG_"n"=y";n=""}' lentils.Kconfig > "$CONFIG"

mapfile -t NAMES < <(awk '/^config/{print$2}' lentils.Kconfig)
TOTAL=${#NAMES[@]}

declare -a STATE
for name in "${NAMES[@]}"; do
    grep -q "^CONFIG_${name}=y" "$CONFIG" && STATE+=("1") || STATE+=("0")
done

tput smcup; tput civis; stty -echo -icanon
SELECTED=0; ROWS=$(tput lines); COLS=$(tput cols)

draw() {
    printf '\033[H\033[J'
    printf '\033[1;37mLentils Configuration\033[0m  (%d)\n' "$TOTAL"
    printf 'j/k/arrows navigate  space toggle  enter save  q quit\n\n'
    ((vis = ROWS - 4, start = SELECTED - vis / 2))
    ((start < 0)) && start=0; ((start+vis > TOTAL)) && ((start = TOTAL - vis))
    ((start < 0)) && start=0
    for ((i=start; i<start+vis && i<TOTAL; i++)); do
        rev=''; ((i == SELECTED)) && rev=$(tput rev)
        m='[ ]'; ((STATE[i] == 1)) && m='[x]'
        printf '%s %-*s\033[0m\n' "$rev" "$((COLS-4))" "$m ${NAMES[i]}"
    done
}

while true; do
    draw
    IFS= read -r -s -n1 key
    # Arrow keys send \033[A etc — read the rest
    if [[ $key == $'\e' ]]; then
        IFS= read -r -s -n1 -t 0.005 k2
        IFS= read -r -s -n1 -t 0.005 k3
        key="$k2$k3"
    fi
    case "$key" in
        q|Q) break ;;
        ' ') ((STATE[SELECTED] ^= 1)) ;;
        ''|$'\n'|$'\r') break 2 ;;  # Enter
        j|J|'[B'|'[C') ((SELECTED < TOTAL-1)) && ((SELECTED++)) ;;
        k|K|'[A'|'[D') ((SELECTED > 0)) && ((SELECTED--)) ;;
    esac
done

stty echo icanon; tput cnorm; tput rmcup

for ((i=0; i<TOTAL; i++)); do
    ((STATE[i] == 1)) && echo "CONFIG_${NAMES[i]}=y" || echo "# CONFIG_${NAMES[i]} is not set"
done > "$CONFIG"

scripts/gen-config.sh "$CONFIG" Lentils/Config/Generated.lean
echo ""
echo "Configuration saved. Run 'lake build' to rebuild."
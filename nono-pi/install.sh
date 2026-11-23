#!/bin/sh
# Install nono-pi (macOS / Linux): links commands into ~/.local/bin, adds the
# `pis` alias, installs the VM driver deps, writes a starter config.
# Then run `nono-pi setup` once (keychain + ssh key passphrase).
set -eu
HERE=$(cd "$(dirname "$0")" && pwd)
BIN=${NONO_PI_BIN:-$HOME/.local/bin}

command -v nono >/dev/null || { echo "nono is not installed: https://nono.sh" >&2; exit 1; }
command -v python3 >/dev/null || { echo "python3 is required" >&2; exit 1; }
command -v zellij >/dev/null || echo "note: zellij not found; nono-pi works without it (prompts use dialogs/terminal)" >&2
if [ "$(uname)" = Linux ] && ! command -v secret-tool >/dev/null; then
  echo "note: secret-tool (libsecret) not found: secrets live only for the session on Linux" >&2
fi

mkdir -p "$BIN"
ln -sf "$HERE/bin/nono-pi" "$BIN/nono-pi"
ln -sf "$HERE/bin/nono-pi-helper" "$BIN/nono-pi-key"
echo "linked $BIN/nono-pi and $BIN/nono-pi-key"

add_line() { # file line
  [ -f "$1" ] || return 0
  grep -qF "$2" "$1" 2>/dev/null || { printf '\n%s\n' "$2" >> "$1"; echo "updated $1"; }
}
if [ -d "$HOME/.config/fish" ]; then
  mkdir -p "$HOME/.config/fish/conf.d"
  printf 'alias pis "nono-pi"\n' > "$HOME/.config/fish/conf.d/nono-pi.fish"
fi
add_line "$HOME/.bashrc" 'alias pis="nono-pi"'
add_line "$HOME/.zshrc" 'alias pis="nono-pi"'

if command -v npm >/dev/null; then
  (cd "$HERE/vm" && npm install --ignore-scripts --no-audit --no-fund --silent) && echo "VM driver deps installed (nono-pi --vm)"
fi

python3 "$HERE/bin/nono-pi" --init-config
echo
echo "next: nono-pi setup     (one-time: nono-pi keychain, ssh key passphrase)"
echo "then: cd <project> && pis"

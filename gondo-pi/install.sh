#!/bin/sh
# Install nono-pi (macOS / Linux): links commands into ~/.local/bin, adds the
# `pis` alias, installs the VM deps (Gondolin), writes a starter config.
# Then run `nono-pi setup` once (keychain + ssh key passphrase).
set -eu
HERE=$(cd "$(dirname "$0")" && pwd)
BIN=${NONO_PI_BIN:-$HOME/.local/bin}

command -v node >/dev/null || { echo "node (>= 22) is required for the microVM daemon" >&2; exit 1; }
if [ "$(uname)" = Linux ] && [ ! -w /dev/kvm ]; then
  echo "note: /dev/kvm is not writable: the microVM needs KVM (add yourself to the kvm group)" >&2
fi
command -v python3 >/dev/null || { echo "python3 is required" >&2; exit 1; }
command -v zellij >/dev/null || echo "note: zellij not found; without it pis runs just pi in the VM (prompts use dialogs/terminal)" >&2
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
  (cd "$HERE/vm" && npm ci --ignore-scripts --no-audit --no-fund --silent && node patch-gondolin.mjs) \
    && echo "VM deps installed (Gondolin)"
fi

python3 "$HERE/bin/nono-pi" --init-config
echo
echo "next: nono-pi setup     (one-time: nono-pi keychain, ssh key passphrase)"
echo "then: cd <project> && pis"

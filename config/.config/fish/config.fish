set -gx EDITOR nvim

bind \ci forward-word
bind \ch backward-word
bind \ce up-or-search

bind \co accept-autosuggestion

bind \cy yank
bind \t complete

set -lx PATH "$HOME/.pyenv/bin:$HOME/.local/share/bin:$HOME/.cask/bin:$PATH:$HOME/.rbenv/bin:$PATH:$HOME/.rbenv/shims:$PATH"

alias ec "emacsclient"
alias mail "neomutt"
alias nv "nvim"

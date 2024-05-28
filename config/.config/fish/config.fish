set -gx EDITOR nvim

bind \ci forward-word
bind \ch backward-word
bind \ce up-or-search

bind \co accept-autosuggestion

bind \cy yank
bind \t complete

set -gx PATH "$HOME/.pyenv/bin:$HOME/.local/share/bin:$HOME/.cask/bin:$HOME/.rbenv/bin:$HOME/.rbenv/shims:$PATH:$HOME/git/newsuk/go/bin"

alias ec "emacsclient"
alias mail "neomutt"
alias nv "nvim"

# Created by `pipx` on 2024-02-12 09:43:57
set PATH $PATH /Users/hugo/.local/bin

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
alias gnv "nvim --listen 127.0.0.1:55432 ."

# Created by `pipx` on 2024-02-12 09:43:57
set PATH $PATH /Users/hugo/.local/bin
set -x VCPKG_ROOT "/Users/hugo/git/vcpkg"
set -x PATH $VCPKG_ROOT $PATH

function saml-login
    source (saml2aws script --shell=fish | psub)
end


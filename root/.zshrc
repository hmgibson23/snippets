# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.

zd() {
    local dir
    dir="$(fasd -Rdl "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}

v() {
    local file
    file="$(fasd -Rfl "$1" | fzf -1 -0 --no-sort +m)" && $EDITOR "${file}" || return 1
}

alias loadrbenv= "$(rbenv init -)"
alias ec="emacsclient -t"
alias k="kubectl"
alias loadnvm="$([ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh")"
alias tmux="TERM=screen-256color-bce tmux"
alias nv="nvim"
# eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"
eval "$(fasd --init auto)"


bindkey -e
bindkey '^R' history-incremental-search-backward
bindkey '^I' emacs-forward-word
bindkey '^H' emacs-backward-word
bindkey '^N' down-line-or-history
bindkey '^E' up-history
#
bindkey -s '^X^Z' '%-^M'
bindkey '^[^I' reverse-menu-complete
bindkey '^X^N' accept-and-infer-next-history
bindkey '^W' kill-region
bindkey '^I' complete-word
## Fix weird sequence that rxvt produces
bindkey -s '^[[Z' '\t'

source <(kubectl completion zsh)  # setup autocomplete in zsh into the current shell

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

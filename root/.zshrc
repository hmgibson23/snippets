export ZSH="/home/hugo/.oh-my-zsh"

ZSH_THEME="fishy"

da() {
  local cid
  cid=$(docker ps -a | sed 1d | fzf -1 -q "$1" | awk '{print $1}')

  [ -n "$cid" ] && docker start "$cid" && docker attach "$cid"
}

ds() {
  local cid
  cid=$(docker ps | sed 1d | fzf -q "$1" | awk '{print $1}')

  [ -n "$cid" ] && docker stop "$cid"
}

zd() {
    local dir
    dir="$(fasd -Rdl "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}

v() {
    local file
    file="$(fasd -Rfl "$1" | fzf -1 -0 --no-sort +m)" && $EDITOR "${file}" || return 1
}

# fh - repeat history
fh() {
  eval $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}

source $ZSH/oh-my-zsh.sh

alias loadrbenv= "rbenv init -"
alias ec="emacsclient -t"
alias k="kubectl"
alias loadnvm='[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"'
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
zle -N fh fh
bindkey '^R' fh
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

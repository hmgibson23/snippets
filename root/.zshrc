ZSH_THEME="fishy"

zd() {
    local dir
    dir="$(fasd -Rdl "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}

v() {
    local file
    file="$(fasd -Rfl "$1" | fzf -1 -0 --no-sort +m)" && $EDITOR "${file}" || return 1
}

ve () {
    local file
    file="$(fasd -Rfl "$1" | fzf -1 -0 --no-sort +m)" && emacsclient "${file}" || return 1
}

source $ZSH/oh-my-zsh.sh

function start_agent {
    echo "Initialising new SSH agent..."
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add;
}

bindkey -s '^[[Z' '\t'

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

# Source SSH settings, if applicable

if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    #ps ${SSH_AGENT_PID} doesn't work under cywgin
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi


alias loadrbenv= "rbenv init -"
alias ec="emacsclient -t"
alias k="kubectl"
alias loadnvm='[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"'
alias tmux="TERM=screen-256color-bce tmux"
alias n="nvim"
alias k_compl="source <(kubectl completion zsh)"
alias set_display_wsl="export DISPLAY=$( awk '/nameserver/ { print $2  }' /etc/resolv.conf  ):0"
alias ed="emacs --daemon"
eval "$(fasd --init auto)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source ~/.commacd.sh

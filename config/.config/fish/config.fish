set -gx EDITOR nvim

bind \ci forward-word
bind \ch backward-word
bind \ce up-or-search

bind \co accept-autosuggestion

bind \cy yank
bind \t complete

alias ff "fasd_cd | fzf"
alias ec "emacsclient"
alias mail "neomutt"
alias nv "nvim"
alias a='fasd -a'        # any
alias s='fasd -si'       # show / search / select
alias d='fasd -d'        # directory
alias f='fasd -f'        # file
alias sd='fasd -sid'     # interactive directory selection
alias sf='fasd -sif'     # interactive file selection
alias z='fasd_cd -d'     # cd, same functionality as j in autojump
alias zz='fasd_cd -d -i' # cd with interactive selection

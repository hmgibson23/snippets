" Writing
setlocal autoindent
setlocal colorcolumn=0
setlocal linebreak
setlocal nonumber
setlocal shiftwidth=4
setlocal spell
setlocal tabstop=4
setlocal wrap
setlocal tw=70

let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_guifg = 'DarkGray'
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!
autocmd! User GoyoEnter set spell
autocmd! User GoyoLeave set spell!
autocmd! User GoyoLeave set spell!
hi clear SpellBad
hi SpellBad cterm=underline ctermfg=red

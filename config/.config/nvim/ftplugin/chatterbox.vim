" Vim filetype plugin for ChatterScript
" Place in ~/.config/nvim/ftplugin/chatterbox.vim
" Optional settings for better ChatterScript editing

" Enable spell checking for dialogue
setlocal spell spelllang=en_us

" Soft wrap for readability
setlocal wrap linebreak

" Use 4 spaces for indentation (for choice responses)
setlocal expandtab
setlocal shiftwidth=4
setlocal tabstop=4

" Auto-close command syntax <<>>
inoremap <buffer> << <><Left>

" Comments
setlocal commentstring=//\ %s

" Navigate between nodes
nnoremap <buffer> <silent> ]] :call search('^===\s*$', 'W')<CR>
nnoremap <buffer> <silent> [[ :call search('^===\s*$', 'bW')<CR>

" Navigate between node titles
nnoremap <buffer> <silent> ]t :call search('^title:', 'W')<CR>
nnoremap <buffer> <silent> [t :call search('^title:', 'bW')<CR>

" Fold nodes (optional)
setlocal foldmethod=expr
setlocal foldexpr=ChatterboxFold(v:lnum)

function! ChatterboxFold(lnum)
    let line = getline(a:lnum)
    " Start fold at node separator
    if line =~ '^===\s*$'
        return '>1'
    endif
    " Continue fold
    return '='
endfunction

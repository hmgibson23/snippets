" Ale
let g:ale_fix_on_save = 1
let g:ale_open_list = 1
let g:ale_sign_error = '✗\ '
let g:ale_sign_warning = '⚠\ '
let g:ale_lint_on_text_changed = 'never'

" lint after 1000ms after changes are made both on insert mode and normal mode
let g:ale_lint_delay = 1000

let g:ale_linters = {
      \  'css':        ['csslint'],
      \  'javascript': ['eslint'],
      \  'json':       ['jsonlint'],
      \  'markdown':   ['mdl', 'aspell'],
      \  'ruby':       ['rubocop'],
      \  'scss':       ['sasslint'],
      \  'yaml':       ['yamllint'],
      \   'go':        ['gometalinter', 'gofmt'],
      \}

" fixer configurations
let g:ale_fixers = {
      \   '*': ['remove_trailing_lines', 'trim_whitespace'],
      \}

" Airline
let g:airline_powerline_fonts = 1

" Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#omni_patterns = {}
let g:deoplete#enable_at_startup = 1
call deoplete#initialize()

" python
autocmd BufWritePre *.py :%s/\s\+$//e

" go
let g:go_fmt_options = ''

function! Multiple_cursors_before()
    let b:deoplete_disable_auto_complete = 1
endfunction
function! Multiple_cursors_after()
    let b:deoplete_disable_auto_complete = 0
endfunction

"Ledger
au BufNewFile,BufRead *.ldg,*.ledger,*.dat setf ledger

" To use fd instead of ag:
command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'fd --type file --follow --color=always'.shellescape(<q-args>), 1,
      \   <bang>0 ? fzf#vim#with_preview('up:60%')
      \           : fzf#vim#with_preview('right:50%:hidden', '?'),
      \   <bang>0)

" Shebangs
augroup Shebang
  autocmd BufNewFile *.py 0put =\"#!/usr/bin/env python\<nl>\"|$
  autocmd BufNewFile *.pl 0put =\"#!/usr/bin/perl \<nl>\"|$
  autocmd BufNewFile *.rb 0put =\"#!/usr/bin/env ruby\<nl>\"|$
  autocmd BufNewFile *.sh 0put =\"#!/usr/bin/env sh\<nl>\"|$
  autocmd BufNewFile *.tex 0put =\"%&plain\<nl>\"|$
  autocmd BufNewFile *.\(cc\|hh\) 0put =\"//\<nl>// \".expand(\"<afile>:t\").\" -- \<nl>//\<nl>\"|2|start!
augroup END

" Change directory
autocmd BufEnter * silent! lcd %:p:h

" deoplete
let g:deoplete#omni#input_patterns = {}
let g:deoplete#omni#input_patterns.lua = '\w+|[^. *\t][.:]\w*'

" yaml
au! BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml foldmethod=indent
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

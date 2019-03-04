" Up/down/left/right
nnoremap h h|xnoremap h h|onoremap h h
nnoremap n j|xnoremap n j|onoremap n j
nnoremap e k|xnoremap e k|onoremap e k
nnoremap i l|xnoremap i l|onoremap i l
" Words forward/backward
nnoremap j e|xnoremap j e|onoremap j e
nnoremap J E|xnoremap J E|onoremap J E

" inSert/Replace/append (T)
nnoremap u i|onoremap u i
nnoremap U I
" Undo/redo
nnoremap l u
nnoremap L U
" Visual mode
" Make insert/add work also in visual line mode like in visual block mode
xnoremap <silent> <expr> u (mode() =~# "[V]" ? "\<C-V>0o$I" : "I")
xnoremap <silent> <expr> U (mode() =~# "[V]" ? "\<C-V>0o$I" : "I")
xnoremap <silent> <expr> a (mode() =~# "[V]" ? "\<C-V>0o$A" : "A")
xnoremap <silent> <expr> A (mode() =~# "[V]" ? "\<C-V>0o$A" : "A")
" Search
nnoremap k n|xnoremap k n|onoremap k n|
nnoremap K N|xnoremap K N|onoremap K N|
" Window handling
nnoremap <C-W>h <C-W>h|xnoremap <C-W>h <C-W>h|
nnoremap <C-W>n <C-W>j|xnoremap <C-W>n <C-W>j|
nnoremap <C-W>e <C-W>k|xnoremap <C-W>e <C-W>k|
nnoremap <C-W>i <C-W>l|xnoremap <C-W>i <C-W>l|
" Operator
inoremap <C-e> <C-o>A
"Colemak specific stuff ends here.

let mapleader = ","

nnoremap <leader>cc :<C-u>Commentary<CR>
"Fzf
nnoremap <leader>fb :<C-u>Buffers<CR>
nnoremap <leader>fc :<C-u>Commands<CR>
nnoremap <leader>fa :<C-u>Ag<CR>
nnoremap <leader>fm :<C-u>Marks<CR>
nnoremap <leader>fh :<C-u>History<CR>
inoremap uu <Esc>

nmap <leader>tp :tabnext<CR>
map <C-p> :Files<CR>
inoremap <silent><expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
inoremap <silent><expr><s-tab> pumvisible() ? "\<c-e>" : "\<s-tab>"

map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)
let g:incsearch#auto_nohlsearch = 1
map k <Plug>(incsearch-nohl)<Plug>(anzu-n-with-echo)
map K <Plug>(incsearch-nohl)<Plug>(anzu-N-with-echo)
map * <Plug>(incsearch-nohl)<Plug>(anzu-star-with-echo)
map # <Plug>(incsearch-nohl)<Plug>(anzu-sharp-with-echo)
let g:anzu_status_format = "%p(%i/%l) %w"
call plug#begin('~/.vim/plugged')

Plug 'neoclide/coc.nvim', {
      \ 'branch': 'release',
      \}
Plug 'airblade/vim-gitgutter'
Plug 'fsharp/vim-fsharp', {
      \ 'for': 'fsharp',
      \ 'do':  'make fsautocomplete',
      \}

Plug 'OmniSharp/omnisharp-vim', { 'for': 'fsharp' }
Plug 'liuchengxu/vim-clap'

Plug 'bronson/vim-trailing-whitespace'
Plug 'editorconfig/editorconfig-vim'
Plug 'mhinz/vim-startify'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries', 'for' : 'go' }
Plug 'easymotion/vim-easymotion'

Plug 'sebdah/vim-delve', { 'for': 'go' }
Plug 'wellle/targets.vim'
Plug 'godoctor/godoctor.vim', { 'for': 'go' }

Plug 'hashivim/vim-terraform', {'for': 'terraform'}
Plug 'haya14busa/incsearch.vim'
Plug 'jiangmiao/auto-pairs'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'junegunn/goyo.vim', { 'for': 'markdown' }
Plug 'junegunn/limelight.vim', { 'for': 'markdown' }

Plug 'ledger/vim-ledger', {'for': 'ledger'}

Plug 'osyo-manga/vim-anzu'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'

Plug 'vim-pandoc/vim-pandoc', {'for': 'markdown'}
Plug 'tpope/vim-markdown', {'for': 'markdown'}

Plug 'vim-scripts/grep.vim'
Plug 'vim-scripts/mru.vim'
Plug 'mhinz/vim-grepper', { 'on': ['Grepper', '<plug>(GrepperOperator)'] }


Plug 'janko-m/vim-test'

Plug 'w0rp/ale'

Plug 'ryanoasis/vim-devicons'
Plug 'vim-airline/vim-airline'

call plug#end()

let $NVIM_TUI_ENABLE_TRUE_COLOR=1

" fix files on save
let g:ale_fix_on_save = 1

" lint after 1000ms after changes are made both on insert mode and normal mode
let g:ale_lint_on_text_changed = 'always'
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
let g:ale_open_list = 1
" use nice symbols for errors and warnings
let g:ale_sign_error = '✗\ '
let g:ale_sign_warning = '⚠\ '

" fixer configurations
let g:ale_fixers = {
      \   '*': ['remove_trailing_lines', 'trim_whitespace'],
      \}

autocmd BufWritePre *.py :%s/\s\+$//e
autocmd VimEnter - if exists(':Dotenv') | exe 'Dotenv! ~/.env.local|Dotenv! ~/.env.'.substitute($DISPLAY, '\.\d\+$', '', '') | endif


set runtimepath+=~/.vim,~/.vim/after
set packpath+=~/.vim

"Turn on backup option
set backup
"Where to store backups
set backupdir=~/.vim/backup//
"Make backup before overwriting the current buffer
set writebackup
"Overwrite the original backup file
set backupcopy=yes
"Meaningful backup name, ex: filename@2015-04-05.14:59
au BufWritePre * let &bex = '@' . strftime("%F.%H:%M")

setglobal wildmenu
setglobal wildmode=full
setglobal wildignore+=tags,.*.un~,*.pyc
set autowrite
set autoread
setglobal history=200
set shiftwidth=2
set visualbell
set noerrorbells
set tabstop=8
setglobal laststatus=2
set expandtab
set autoindent
set smartindent
set cindent
set hlsearch
set incsearch
set ignorecase
set smartcase

setglobal grepformat=%f:%l:%c:%m,%f:%l:%m,%f:%l%m,%f\ \ %l%m
if executable('ag')
        setglobal grepprg=ag\ -s\ --vimgrep
  elseif has('unix')
          setglobal grepprg=grep\ -rn\ $*\ /dev/null
endif

filetype plugin indent on
syntax on

source ~/.config/nvim/keybindings.vim
source ~/.config/nvim/aliases.vim
source  ~/.config/nvim/general.vim
source ~/.config/nvim/functions.vim
source ~/.config/nvim/coc.vim

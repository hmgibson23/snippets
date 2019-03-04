call plug#begin('~/.vim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'airblade/vim-gitgutter'
Plug 'bronson/vim-trailing-whitespace'
Plug 'markonm/traces.vim'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries', 'for' : 'go' }
Plug 'easymotion/vim-easymotion'
Plug 'sebdah/vim-delve', { 'for': 'go' }
Plug 'godoctor/godoctor.vim', { 'for': 'go' }
Plug 'hashivim/vim-terraform', {'for': 'terraform'}
Plug 'haya14busa/incsearch.vim'
Plug 'jceb/vim-orgmode', { 'for': 'org' }
Plug 'jiangmiao/auto-pairs'
Plug 'jodosha/vim-godebug', { 'for': 'go' }
Plug 'juliosueiras/vim-terraform-completion'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim', { 'for': 'markdown' }
Plug 'junegunn/limelight.vim', { 'for': 'markdown' }
Plug 'kassio/neoterm'
Plug 'ledger/vim-ledger', {'for': 'ledger'}
Plug 'osyo-manga/vim-anzu'
Plug 'sheerun/vim-polyglot'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'radenling/vim-dispatch-neovim'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-abolish'
Plug 'jamessan/vim-gnupg', {'for' : 'gpg'}
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown', {'for': 'markdown'}
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'mhinz/vim-grepper', { 'on': ['Grepper', '<plug>(GrepperOperator)'] }
Plug 'vim-airline/vim-airline'
Plug 'vim-pandoc/vim-pandoc', {'for': 'markdown'}
Plug 'vim-scripts/grep.vim'
Plug 'janko-m/vim-test'
Plug 'w0rp/ale'
Plug 'zchee/deoplete-go', {'build': {'unix': 'make'}}
Plug 'ryanoasis/vim-devicons'

call plug#end()

let $NVIM_TUI_ENABLE_TRUE_COLOR=1

let g:deoplete#enable_at_startup = 1

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

let g:airline_powerline_fonts = 1
let g:deoplete#omni_patterns = {}
let g:deoplete#omni_patterns.terraform = '[^ *\t"{=$]\w*'
let g:deoplete#enable_at_startup = 1
call deoplete#initialize()

autocmd BufWritePre *.py :%s/\s\+$//e

set runtimepath+=~/.vim,~/.vim/after
set packpath+=~/.vim
set wildmenu
set autowrite
set autochdir
set shiftwidth=2
set visualbell
set noerrorbells
set tabstop=8
set expandtab
set autoindent
set smartindent
set cindent
set hlsearch
set incsearch
set ignorecase
set smartcase
filetype plugin indent on
syntax on

source ~/.config/nvim/keybindings.vim
source ~/.config/nvim/aliases.vim
source  ~/.config/nvim/gorc

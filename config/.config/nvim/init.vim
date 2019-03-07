call plug#begin('~/.vim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'airblade/vim-gitgutter'
Plug 'bronson/vim-trailing-whitespace'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'easymotion/vim-easymotion'
Plug 'editorconfig/editorconfig-vim'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries', 'for' : 'go' }
Plug 'godoctor/godoctor.vim', { 'for': 'go' }
Plug 'hashivim/vim-terraform', {'for': 'terraform'}
Plug 'haya14busa/incsearch.vim'
Plug 'jamessan/vim-gnupg', {'for' : 'gpg'}
Plug 'janko-m/vim-test'
Plug 'jceb/vim-orgmode', { 'for': 'org' }
Plug 'jiangmiao/auto-pairs'
Plug 'jodosha/vim-godebug', { 'for': 'go' }
Plug 'juliosueiras/vim-terraform-completion'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim', { 'for': 'markdown' }
Plug 'junegunn/limelight.vim', { 'for': 'markdown' }
Plug 'ledger/vim-ledger', {'for': 'ledger'}
Plug 'markonm/traces.vim'
Plug 'mhinz/vim-grepper', { 'on': ['Grepper', '<plug>(GrepperOperator)'] }
Plug 'osyo-manga/vim-anzu'
Plug 'radenling/vim-dispatch-neovim'
Plug 'scrooloose/vim-slumlord', { 'for': 'plantuml' }
Plug 'sebdah/vim-delve', { 'for': 'go' }
Plug 'sheerun/vim-polyglot'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown', {'for': 'markdown'}
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-pandoc/vim-pandoc', {'for': 'markdown'}
Plug 'w0rp/ale'
Plug 'zchee/deoplete-go', {'build': {'unix': 'make'}}
Plug 'ryanoasis/vim-devicons'

call plug#end()

let $NVIM_TUI_ENABLE_TRUE_COLOR=1


set runtimepath+=~/.vim,~/.vim/after
set packpath+=~/.vim
set wildmenu
set autowrite
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

source ~/.config/nvim/functions.vim
source ~/.config/nvim/keybindings.vim
source ~/.config/nvim/aliases.vim
source  ~/.config/nvim/general.vim

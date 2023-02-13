-- let $NVIM_TUI_ENABLE_TRUE_COLOR=1
-- vim.cmd [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()]]
-- -- or
-- vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()]]
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.cmd([[
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
set hidden
set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes


setglobal grepformat=%f:%l:%c:%m,%f:%l:%m,%f:%l%m,%f\ \ %l%m
if executable('ag')
        setglobal grepprg=ag\ -s\ --vimgrep
  elseif has('unix')
          setglobal grepprg=grep\ -rn\ $*\ /dev/null
endif

filetype plugin indent on
syntax on

source ~/.config/nvim/aliases.vim
source  ~/.config/nvim/general.vim
source ~/.config/nvim/functions.vim
]])
require("keybindings")

require("plugins")

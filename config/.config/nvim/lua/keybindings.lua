local map = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }
local xpr = { noremap = true, expr = true }

vim.g.mapleader = ","

-- Telescope
map("n", "<c-P>", "<cmd>Telescope find_files<cr>", opts)
map("n", "<c-T>", "<cmd>Telescope find_files<cr>", opts)
map("n", "<leader>ff", "<cmd>Telescope find_files<cr>", opts)
map("n", "<leader>fb", "<cmd>Telescope buffers<cr>", opts)
map("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", opts)
map("n", "<leader>fr", "<cmd>Telescope registers<cr>", opts)
map("n", "<leader>ft", "<cmd>Telescope treesitter<cr>", opts)
map("n", "<leader>fm", "<cmd>Telescope man_pages<cr>", opts)
map("n", "<leader>fo", "<cmd>Telescope oldfiles<cr>", opts)
map("n", "<leader>fc", ":<C-u>Ag<cr>", opts)

map("n", "<leader>tt", ":ToggleTerm<cr>", opts)

map("n", "<leader>tp", "<cmd>TSPlaygroundToggle<cr>", opts)

vim.cmd([[inoremap uu <Esc>|tnoremap uu <C-\><C-n>
" map /  <Plug>(incsearch-forward)
" map ?  <Plug>(incsearch-backward)
" map g/ <Plug>(incsearch-stay)
" let g:incsearch#auto_nohlsearch = 1
map k <Plug>(incsearch-nohl)<Plug>(anzu-n-with-echo)
map K <Plug>(incsearch-nohl)<Plug>(anzu-N-with-echo)
map * <Plug>(incsearch-nohl)<Plug>(anzu-star-with-echo)
map # <Plug>(incsearch-nohl)<Plug>(anzu-sharp-with-echo)

]])

vim.cmd([[
map *   <Plug>(asterisk-*)
map #   <Plug>(asterisk-#)
map g*  <Plug>(asterisk-g*)
map g#  <Plug>(asterisk-g#)
map z*  <Plug>(asterisk-z*)
map gz* <Plug>(asterisk-gz*)
map z#  <Plug>(asterisk-z#)
map gz# <Plug>(asterisk-gz#)
]])
vim.g.anzu_status_format = "%p(%i/%l) %w"
-- Specials
-- map('i', '<Esc>', 'uu <C-\><C-n>', opts)
-- Colemak Keybindings {{
----------------------
map("n", "n", "j", opts)
map("x", "n", "j", opts)
map("o", "n", "j", opts)
map("n", "e", "k", opts)
map("x", "e", "k", opts)
map("o", "e", "k", opts)
map("n", "i", "l", opts)
map("x", "i", "l", opts)
map("o", "i", "l", opts)

-- Colemak Insert
map("n", "u", "i", opts)
map("n", "U", "I", opts)
map("x", "u", "i", opts)
map("x", "U", "I", opts)
map("o", "u", "i", opts)
map("o", "U", "I", opts)

-- Undo/redo
map("n", "l", "u", opts)
map("x", "l", ":<C-U>undo<CR>", opts)
map("n", "gl", "u", opts)
map("x", "gl", ":<C-U>undo<CR>", opts)
--  map('n', 'Z', '<C-R>', opts)
--  map('x', 'Z', ':<C-U>undo<CR>', opts)

-- Colemak Windows
map("n", "<C-W>h", "<C-W>h", opts)
map("n", "<C-W>n", "<C-W>j", opts)
map("n", "<C-W>e", "<C-W>k", opts)
map("n", "<C-W>i", "<C-W>l", opts)
map("x", "<C-W>h", "<C-W>h", opts)
map("x", "<C-W>n", "<C-W>j", opts)
map("x", "<C-W>e", "<C-W>k", opts)
map("x", "<C-W>i", "<C-W>l", opts)

-- window & tab controls
map("n", "<leader>ss", ":sp<space>", opts)
map("n", "<leader>vs", ":vsp<space>", opts)
-- tab controls -- ctrl-t makes a new tab
map("n", "<C-t>", "<Esc>:tabnew<CR>", opts) -- Check collision!
-- shift T turn a split window into a tab
map("n", "<S-T>", "<Esc><C-w>T", opts) -- Check collision!
map("n", "te", ":tabnext<CR>", opts)
map("n", "tn", ":tabprev<CR>", opts)
map("n", "th", ":tabfirst<CR>", opts)
map("n", "ti", ":tablast<CR>", opts)
map("n", "<leader>mm", ":Git<CR>", opts)
map("n", "cd", ":lcd %:p:h<CR>", opts)

-- }}}

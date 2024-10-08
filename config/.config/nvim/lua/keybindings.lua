local map = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

vim.g.mapleader = ","

vim.cmd([[inoremap uu <Esc>|tnoremap uu <C-\><C-n>]])
vim.g.anzu_status_format = "%p(%i/%l) %w"
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
-- tab controls -- ctrl-t makes a new tab
map("n", "<C-t>", "<Esc>:tabnew<CR>", opts) -- Check collision!
-- shift T turn a split window into a tab
map("n", "<S-T>", "<Esc><C-w>T", opts) -- Check collision!
map("n", "te", ":tabnext<CR>", opts)
map("n", "tn", ":tabprev<CR>", opts)
map("n", "th", ":tabfirst<CR>", opts)
map("n", "ti", ":tablast<CR>", opts)
map("n", "<leader>mm", ":Neogit<CR>", opts)
map("n", "cd", ":lcd %:p:h<CR>", opts)

-- }}}

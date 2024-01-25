local M = {}
local keymap = vim.keymap.set
local default_opts = { noremap = true, silent = true }

M.setup = function()
	require("legendary").setup({
		which_key = { auto_register = true },
	})
	keymap("n", "<C-p>", "<cmd>lua require('legendary').find()<CR>", default_opts)
end

return M

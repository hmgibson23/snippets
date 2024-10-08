local M = {}

function M.setup()
	require("toggleterm").setup({
		hide_numbers = false,
		shade_terminals = true,
		direction = "vertical",
		size = 60,
		winbar = {
			enabled = false,
			name_formatter = function(term)
				return term.name
			end,
		},
	})
	local python_term =
		require("toggleterm.terminal").Terminal:new({ cmd = "python3", hidden = true, direction = "float" })
	vim.keymap.set("n", "<C-p>", function()
		python_term:toggle()
	end, { noremap = true, silent = true })
	local function markdown_codeblock(language, content)
		return "\\`\\`\\`{" .. language .. "}\n" .. content .. "\n\\`\\`\\`"
	end

	local quarto_notebook_cmd = 'nvim -c enew -c "set filetype=quarto"'
		.. ' -c "norm GO## IPython\nThis is Quarto IPython notebook. Syntax is the same as in markdown\n\n'
		.. markdown_codeblock("python", "# enter code here\n")
		.. '"'
		.. ' -c "norm Gkk"'
		-- This line needed because QuartoActivate and MoltenInit commands must be accessible; should be adjusted depending on plugin manager
		.. " -c \"lua require('lazy.core.loader').load({'molten-nvim', 'quarto-nvim'}, {cmd = 'Lazy load'})\""
		.. ' -c "MoltenInit python3" -c QuartoActivate -c startinsert'
	local molten_term = require("toggleterm.terminal").Terminal:new({
		cmd = quarto_notebook_cmd,
		hidden = true,
		direction = "float",
	})
	vim.keymap.set("n", "<C-p>", function()
		molten_term:toggle()
	end, { noremap = true, silent = true })
	vim.keymap.set("t", "<C-p>", function()
		vim.cmd("stopinsert")
		molten_term:toggle()
	end, { noremap = true, silent = true })
end

return M

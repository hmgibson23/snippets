local M = {}

function M.setup()
	local quarto = require("quarto")

	quarto.setup({
		lspFeatures = {
			languages = { "r", "python", "rust" },
			chunks = "all",
			diagnostics = {
				enabled = true,
				triggers = { "BufWritePost" },
			},
			completion = {
				enabled = true,
			},
		},
		keymap = {
			hover = "H",
			definition = "gd",
			rename = "<leader>rn",
			references = "gr",
			format = "<leader>gf",
		},
		codeRunner = {
			enabled = true,
			default_method = "molten",
			ft_runners = { python = "molten" },
		},
		plotPreview = {
			enabled = true,
			method = "browser",
		},
	})

	-- Function to get the current chunk tag based on cursor position
	function get_current_cell_tag()
		local current_line = vim.fn.line(".")
		local lines = vim.fn.getline(1, "$") -- Get all lines in the buffer
		local tag = ""

		-- Scan for chunk delimiters like ```{python} or ```{r}
		for i = current_line, 1, -1 do
			local line = lines[i]
			-- Match the opening of a code chunk with a tag
			local match = string.match(line, "```%s*{([a-zA-Z0-9_-]+)}")
			if match then
				tag = match
				break
			end
		end

		return tag
	end

	-- Function to run Quarto render for the current chunk (cell)
	function render_current_cell()
		local tag = get_current_cell_tag()
		if tag == "" then
			print("No chunk tag found at the current cursor position.")
			return
		end

		-- Execute the Quarto render command asynchronously with the tag
		vim.fn.jobstart("quarto render --filter " .. tag, {
			on_stdout = function(_, data)
				for _, line in ipairs(data) do
					print(line)
				end
			end,
			on_stderr = function(_, data)
				for _, line in ipairs(data) do
					print("ERROR: " .. line)
				end
			end,
			on_exit = function(_, code)
				if code == 0 then
					print("Rendered cell: " .. tag)
				else
					print("Failed to render cell: " .. tag)
				end
			end,
		})
	end

	-- Keybinding to trigger render for the current cell
	vim.api.nvim_set_keymap("n", "<leader>qrc", ":lua render_current_cell()<CR>", { noremap = true, silent = true })
end

return M

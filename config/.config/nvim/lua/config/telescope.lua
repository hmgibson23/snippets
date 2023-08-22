local M = {}

local icons = require("config.icons")
local whichkey = require("which-key")
function M.setup()
	local telescope = require("telescope")
	telescope.setup({
		defaults = {
			prompt_prefix = icons.ui.Telescope .. " ",
			selection_caret = " ",
			file_ignore_patterns = {
				".git/",
				".cache",
				"%.o",
				"%.a",
				"%.out",
				"%.class",
				"%.pdf",
				"%.mkv",
				"%.mp4",
				"%.zip",
			},
			border = {},
			borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
			color_devicons = true,
		},
		pickers = {
			find_files = {
				theme = "ivy",
				previewer = false,
				hidden = true,
			},
			buffers = {
				theme = "ivy",
				previewer = false,
				show_all_buffers = true,
			},
		},
	})

	-- telescope.load_extension("smart_history")
	telescope.load_extension("fzf")
	telescope.load_extension("frecency")
	telescope.load_extension("repo")
	telescope.load_extension("cder")
	-- telescope.load_extension("projects")
	whichkey.register({
		fl = {
			name = "Telescope LSP",
			d = { "<cmd>Telescope lsp_definitions<cr>", "LSP Definitions" },
			w = { "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", "LSP Dynamic Workspace Symbols" },
			t = { "<cmd>Telescope lsp_type_definitions<cr>", "LSP Symbols" },
			r = { "<cmd>Telescope lsp_references<cr>", "LSP References" },
			i = { "<cmd>Telescope lsp_implementations<cr>", "LSP Implementations" },
			x = { "<cmd>Telescope lsp_implementations<cr>", "Diagnostics" },
			y = { "<cmd>Telescope treesitter<cr>", "Treesitter" },
		},
	}, { prefix = "<leader>" })

	whichkey.register({
		fx = {
			name = "Telescope X",
			c = { "<cmd>Telescope cder<cr>", "Cder" },
			r = { "<cmd>Telescope repo<cr>", "Repo" },
		},
	}, { prefix = "<leader>" })
end

return M

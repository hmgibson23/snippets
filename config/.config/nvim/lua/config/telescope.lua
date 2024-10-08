local M = {}

local icons = require("config.icons")
local whichkey = require("which-key")
local telescope = require("telescope")

function M.setup()
	telescope.setup({
		defaults = {
			prompt_prefix = icons.ui.Telescope .. " ",
			selection_caret = " ",
			file_ignore_patterns = {
				-- ".git/",
				"^./.git/",
				".cache",
				"%.o",
				"%.a",
				"%.out",
				"%.class",
				"%.pdf",
				"%.mkv",
				"%.mp4",
				"%.zip",
				"node_modules",
				"data_out",
			},
			border = {},
			borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
			color_devicons = true,
			layout_strategy = "bottom_pane", -- Ensure Telescope opens from the bottom
			layout_config = {
				bottom_pane = {
					height = 0.3, -- Adjust the height as needed
				},
			},
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
			frecency = {
				theme = "ivy",
				previewer = false,
				-- other settings if needed
			},
		},
		extensions = {
			frecency = {},
			extensions = {
				whaler = {
					-- Whaler configuration
					directories = {
						{ path = "~/git", alias = "Git Projects" },
					},
				},
			},
		},
	})

	-- Load Telescope extensions
	telescope.load_extension("fzf")
	telescope.load_extension("frecency")
	telescope.load_extension("repo")
	telescope.load_extension("cder")
	telescope.load_extension("whaler")
	telescope.load_extension("projections")
	telescope.load_extension("projects")

	-- Define key mappings for Telescope commands
	-- c = {
	-- 	name = "Commands",
	-- 	c = { "<cmd>Telescope commands<cr>", "Command Completion" },
	-- },
	whichkey.add({

		{ "<leader>f", group = "Telescope" },
		{ "<leader>fx", group = "Telescope X" },
		{ "<leader>fl", group = "Telescope LSP" },
		{ "<leader>fld", "<cmd>Telescope lsp_definitions<cr>", desc = "Telescope LSP definitions", mode = "n" },
		{
			"<leader>flw",
			"<cmd>Telescope lsp_dynamic_workspace_symbols<cr>",
			desc = "Telescope LSP Dynamic Workspace Symbols",
			mode = "n",
		},
		{
			"<leader>flt",
			"<cmd>Telescope lsp_type_definitions<cr>",
			desc = "Telescope LSP Type Definitions",
			mode = "n",
		},
		{ "<leader>flr", "<cmd>Telescope lsp_references<cr>", desc = "Telescope LSP References", mode = "n" },
		{ "<leader>fli", "<cmd>Telescope lsp_implementations<cr>", desc = "Telescope LSP Implementations", mode = "n" },
		{
			"<leader>flx",
			"<cmd>Telescope lsp_workspace_diagnostics<cr>",
			desc = "Telescope LSP Diagnostics",
			mode = "n",
		},

		{ "<leader>fxc", "<cmd>Telescope cder<cr>", desc = "Telescope X cder", mode = "n" },
		{ "<leader>fxr", "<cmd>Telescope repo<cr>", desc = "Telescope X repo", mode = "n" },
		{ "<leader>fxf", "<cmd>Telescope frecency<cr>", desc = "Telescope X Recent Files", mode = "n" },

		{ "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find File", mode = "n" },
		{ "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Find Text", mode = "n" },
		{ "<leader>fr", "<cmd>Telescope registers<cr>", desc = "Registers", mode = "n" },
		{ "<leader>ft", "<cmd>Telescope treesitter<cr>", desc = "Treesitter", mode = "n" },
		{ "<leader>fo", "<cmd>Telescope oldfiles<cr>", desc = "Old Files", mode = "n" },
		{ "<leader>fm", "<cmd>Telescope man_pages<cr>", desc = "Man pages", mode = "n" },
		{ "<leader>fw", "<cmd>Telescope whaler<cr>", desc = "Whaler", mode = "n" },
		{ "<leader>fs", "<cmd>SpectacleTelescope<cr>", desc = "Spectable", mode = "n" },
		{ "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Find buffers", mode = "n" },
	})

	vim.api.nvim_create_user_command("EditConfig", function()
		require("telescope.builtin").find_files({
			prompt_title = "< NVIM CONFIG >",
			cwd = "~/.config/nvim",
			hidden = true,
		})
	end, {})

	-- Optionally bind the command to a key
	whichkey.add({
		{ "<leader>ec", "<cmd>EditConfig<cr>", desc = "Edit Config", mode = "n" },
	})
end

return M

local M = {}

function M.setup()
	require("ts_context_commentstring").setup({})
  -- require("ts")
	local swap_next, swap_prev = (function()
		local swap_objects = {
			p = "@parameter.inner",
			f = "@function.outer",
			c = "@class.outer",
		}

		local n, p = {}, {}
		for key, obj in pairs(swap_objects) do
			n[string.format("<Leader>cx%s", key)] = obj
			p[string.format("<Leader>cX%s", key)] = obj
		end

		return n, p
	end)()

	require("nvim-treesitter.configs").setup({
		-- One of "all", "maintained" (parsers with maintainers), or a list of languages
		ensure_installed = "all",

		-- Install languages synchronously (only applied to `ensure_installed`)
		sync_install = false,

		highlight = {
			-- `false` will disable the whole extension
			enable = true,
		},

		rainbow = {
			enable = true,
			extended_mode = true,
			max_file_lines = nil,
		},

		incremental_selection = {
			enable = true,
			keymaps = {
				init_selection = "gnn",
				node_incremental = "gnr",
				scope_incremental = "gnc",
				node_decremental = "gnm",
			},
		},

		indent = { enable = true, disable = { "python", "java", "rust", "lua" } },

		-- vim-matchup
		matchup = {
			enable = true,
		},

		-- nvim-treesitter-textsubjects
		textsubjects = {
			enable = true,
			prev_selection = ",", -- (Optional) keymap to select the previous selection
			keymaps = {
				["."] = "textsubjects-smart",
				[";"] = "textsubjects-container-outer",
				["i;"] = "textsubjects-container-inner",
			},
		},

		-- nvim-treesitter-textobjects
		textobjects = {
			select = {
				enable = true,

				-- Automatically jump forward to textobj, similar to targets.vim
				lookahead = true,

				keymaps = {
					-- You can use the capture groups defined in textobjects.scm
					["af"] = "@function.outer",
					["if"] = "@function.inner",
					["ac"] = "@class.outer",
					["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
					["ib"] = { query = "@code_cell.inner", desc = "in block" },
					["ab"] = { query = "@code_cell.outer", desc = "around block" },
				},
				selection_modes = {
					["@parameter.outer"] = "v", -- charwise
					["@function.outer"] = "V", -- linewise
					["@class.outer"] = "<c-v>", -- blockwise
				},
			},

			swap = {
				enable = true,

				-- xt = {
				--   ["<leader>cx"] = "@parameter.inner",
				-- },
				-- swap_previous = {
				--   ["<leader>cX"] = "@parameter.inner",
				swap_next = {
					--- ... other keymap
					["<leader>sbl"] = "@code_cell.outer",
					swap_previous = {
						--- ... other keymap
						["<leader>sbh"] = "@code_cell.outer",
					},
				}, -- },
			},

			move = {
				enable = true,
				set_jumps = true, -- whether to set jumps in the jumplist
				goto_next_start = {
					["]m"] = "@function.outer",
					["]]"] = "@class.outer",
				},
				goto_next_end = {
					["]M"] = "@function.outer",
					["]["] = "@class.outer",
				},
				goto_previous_start = {
					["[m"] = "@function.outer",
					["[["] = "@class.outer",
				},
				goto_previous_end = {
					["[M"] = "@function.outer",
					["[]"] = "@class.outer",
				},
			},

			-- lsp_interop = {
			--   enable = true,
			--   border = "none",
			--   peek_definition_code = {
			--     ["<leader>cf"] = "@function.outer",
			--     ["<leader>cF"] = "@class.outer",
			--   },
			-- },
		},

		-- endwise
		-- endwise = {
		-- 	enable = true,
		-- },

		-- autotag
		-- autotag = {
		-- 	enable = true,
		-- },

		-- indent
		-- yati = { enable = true },

		playground = {
			enable = true,
			disable = {},
			updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
			persist_queries = false, -- Whether the query persists across vim sessions
		},

		query_linter = {
			enable = true,
			use_virtual_text = true,
			lint_events = { "BufWrite", "CursorHold" },
		},

		-- markid
		-- markid = { enable = true },
	})
end

return M

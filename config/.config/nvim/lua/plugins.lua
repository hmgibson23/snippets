---@module Plugins
---@author Hugo Gibson
require("lazy").setup({
	{
		"romgrk/kirby.nvim",
		dependencies = {
			{ "romgrk/fzy-lua-native", build = "make install" },
			{ "romgrk/kui.nvim" },
			{ "nvim-tree/nvim-web-devicons" },
			{ "nvim-lua/plenary.nvim" },
		},
	},
	-- {
	-- 	"nvim-neorg/neorg",
	-- 	-- build = ":Neorg sync-parsers",
	-- 	lazy = false, -- specify lazy = false because some lazy.nvim distributions set lazy = true by default
	-- 	ft = "norg",
	-- 	-- tag = "*",
	-- 	dependencies = { "nvim-lua/plenary.nvim", { "nvim-neorg/neorg-telescope" } },
	-- 	config = function()
	-- 		require("neorg").setup({
	-- 			load = {
	-- 				["core.defaults"] = {}, -- Loads default behaviour
	-- 				["core.completion"] = { config = { engine = "nvim-cmp", name = "[Norg]" } },
	-- 				["core.integrations.nvim-cmp"] = {},
	-- 				["core.integrations.telescope"] = {},
	-- 				["core.concealer"] = { config = { icon_preset = "diamond" } },
	-- 				["core.keybinds"] = {
	-- 					config = {
	-- 						default_keybinds = true,
	-- 						neorg_leader = "<Leader><Leader>",
	-- 					},
	-- 				},
	-- 				["core.esupports.metagen"] = { config = { type = "auto", update_date = true } },
	-- 				["core.qol.toc"] = {},
	-- 				["core.qol.todo_items"] = {},
	-- 				["core.looking-glass"] = {},
	-- 				["core.presenter"] = { config = { zen_mode = "zen-mode" } },
	-- 				["core.export"] = {},
	-- 				["core.export.markdown"] = { config = { extensions = "all" } },
	-- 				["core.summary"] = {},
	-- 				["core.tangle"] = { config = { report_on_empty = false } },
	-- 				-- ["core.ui.calendar"] = {},
	-- 				-- ["external.context"] = {},
	-- 				["core.journal"] = {
	-- 					config = {
	-- 						strategy = "flat",
	-- 						workspace = "Notes",
	-- 					},
	-- 				},
	-- 				["core.dirman"] = { -- Manages Neorg workspaces
	-- 					config = {
	-- 						workspaces = {
	-- 							notes = "~/notes",
	-- 						},
	-- 					},
	-- 				},
	-- 			},
	-- 		})
	-- 	end,
	-- },
	{
		"mrjones2014/legendary.nvim",
		priority = 10000,
		lazy = false,
		config = function()
			require("config.legendary").setup()
		end,
	},
	{
		"tadmccorkle/markdown.nvim",
		ft = "markdown", -- or 'event = "VeryLazy"'
		opts = {
			-- configuration here or empty for defaults
		},
		config = function(_, opts)
			require("markdown").setup(opts)
		end,
	},
	{
		"GCBallesteros/jupytext.nvim",
		config = function()
			require("jupytext").setup({
				style = "markdown",
				output_extension = "md",
				force_ft = "markdown",
				fmt = "py",
			})
		end,
	},
	{
		"Civitasv/cmake-tools.nvim",
		config = function()
			require("config.cmake").setup()
		end,
	},
	-- { "indrets/diffview.nvim" },
	{ "shaunsingh/nord.nvim" },
	{
		"anuvyklack/hydra.nvim",
		config = function()
			require("config.hydra").setup()
		end,
	},
	{ "savq/melange-nvim" },
	"filipdutescu/renamer.nvim",
	"wbthomason/packer.nvim",
	{
		"p00f/clangd_extensions.nvim",
		config = function()
			require("config.clangd").setup()
		end,
	},
	{ "lewis6991/impatient.nvim" },
	{ "folke/neoconf.nvim", cmd = "Neoconf" },
	-- { "dccsillag/magma-nvim", build = ":UpdateRemotePlugins" },
	-- { "luk400/vim-jukit" },
	{
		"benlubas/molten-nvim",
		build = ":UpdateRemotePlugins",
		ft = { "markdown" },
		init = function()
			require("config.molten").setup()
		end,
		config = function()
			require("config.molten").setup()
		end,
	},
	{
		"jmbuhr/otter.nvim",
		opts = {
			buffers = {
				set_filetype = true,
			},
		},
	},
	{
		"hedyhli/outline.nvim",
		config = function()
			require("outline").setup({
				-- Your setup opts here (leave empty to use defaults)
			})
		end,
	},
	{
		"roobert/hoversplit.nvim",
		config = function()
			require("hoversplit").setup()
		end,
	},
	{
		"dnlhc/glance.nvim",
		config = function()
			require("glance").setup({})
		end,
	},
	{
		"amrbashir/nvim-docs-view",
		lazy = true,
		cmd = "DocsViewToggle",
		opts = {
			position = "right",
			width = 60,
		},
	},
	{
		"zeioth/garbage-day.nvim",
		dependencies = "neovim/nvim-lspconfig",
		event = "VeryLazy",
	},
	-- {
	-- 	"robitx/gp.nvim",
	-- 	config = function()
	-- 		require("gp").setup()
	-- 	end,
	-- },
	{ "stevanmilic/nvim-lspimport" },
	{
		"quarto-dev/quarto-nvim",
		config = function()
			local quarto = require("quarto")
			quarto.setup({
				lspFeatures = {
					-- NOTE: put whatever languages you want here:
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
					-- NOTE: setup your own keymaps:
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
			})
		end,
	},
	{
		-- see the image.nvim readme for more information about configuring this plugin
		"3rd/image.nvim",
		config = function()
			require("image").setup()
			package.path = package.path .. ";" .. vim.fn.expand("$HOME") .. "/.luarocks/share/lua/5.1/?/init.lua"
			package.path = package.path .. ";" .. vim.fn.expand("$HOME") .. "/.luarocks/share/lua/5.1/?.lua"
		end,
		opts = {
			backend = "kitty", -- whatever backend you would like to use
			max_width = 200,
			max_height = 24,
			max_height_window_percentage = math.huge,
			max_width_window_percentage = math.huge,
			window_overlap_clear_enabled = true, -- toggles images when windows are overlapped
			window_overlap_clear_ft_ignore = { "cmp_menu", "cmp_docs", "" },
		},
	},
	{
		"jcdickinson/codeium.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"hrsh7th/nvim-cmp",
		},
		config = function()
			require("codeium").setup({})
		end,
	},
	{
		"nvim-tree/nvim-web-devicons",
		config = function()
			require("nvim-web-devicons").setup({ default = true })
		end,
	},

	{
		"kevinhwang91/nvim-ufo",
		lazy = true,
		keys = { "zc", "zo", "zR", "zm" },
		dependencies = "kevinhwang91/promise-async",
		config = function()
			require("ufo").setup({
				provider_selector = function(_, _)
					return { "treesitter", "indent" }
				end,
			})
			vim.keymap.set("n", "zR", require("ufo").openAllFolds)
			vim.keymap.set("n", "zM", require("ufo").closeAllFolds)
		end,
		enabled = false,
	},

	{
		"nvim-tree/nvim-tree.lua",
		cmd = { "NvimTreeToggle", "NvimTreeClose" },
		config = function()
			require("config.nvimtree").setup()
		end,
	},
	{
		"lukas-reineke/headlines.nvim",
		dependencies = "nvim-treesitter/nvim-treesitter",
		config = true, -- or `opts = {}`
	},
	{
		"goolord/alpha-nvim",
		config = function()
			require("config.alpha").setup()
		end,
	},
	{
		"echasnovski/mini.nvim",
		event = { "BufReadPre" },
		config = function()
			require("mini.align").setup()
			require("mini.test").setup()
			require("mini.doc").setup()
		end,
	},
	{
		"TaDaa/vimade",
		cmd = { "VimadeToggle", "VimadeEnable", "VimadeDisable" },
		-- enabled = false,
		config = function()
			vim.g.vimade.fadelevel = 0.7
			vim.g.vimade.enablesigns = 1
		end,
	},
	{
		"nvim-neotest/neotest",
		dependencies = {
			{
				"vim-test/vim-test",
				cmd = {
					"TestLast",
					"TestSuite",
					"TestNearest",
					"TestFile",
				},
				config = function()
					require("config.test").setup()
				end,
			},
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter",
			{ "nvim-neotest/neotest-vim-test" },
			{ "nvim-neotest/neotest-python" },
			{ "nvim-neotest/neotest-plenary" },
			{ "nvim-neotest/neotest-go" },
			{ "haydenmeade/neotest-jest" },
			{ "rouge8/neotest-rust" },
		},
		config = function()
			require("config.neotest").setup()
		end,
	},
	"b0o/schemastore.nvim",
	{ "nvim-neotest/nvim-nio" },
	-- LazySpec (plugin specification)
	-- return {
	{
		"dasupradyumna/launch.nvim",
		-- add below plugins as per user requirement
		dependencies = {
			"mfussenegger/nvim-dap",
			"rcarriga/nvim-notify",
		},
	},
	{
		"pianocomposer321/officer.nvim",
		dependencies = "stevearc/overseer.nvim",
		config = function()
			require("officer").setup({
				-- config
			})
		end,
	},
	{
		"stevearc/overseer.nvim",
		cmd = {
			"OverseerToggle",
			"OverseerOpen",
			"OverseerRun",
			"OverseerBuild",
			"OverseerClose",
			"OverseerLoadBundle",
			"OverseerSaveBundle",
			"OverseerDeleteBundle",
			"OverseerRunCmd",
			"OverseerQuickAction",
			"OverseerTaskAction",
		},
		config = function()
			require("config.overseer").setup()
		end,
	},
	{
		"hkupty/iron.nvim",
		config = function()
			require("config.iron").setup()
		end,
	},
	{
		"lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup()
		end,
	},
	-- use({
	-- 	"is0n/jaq-nvim",
	-- 	config = function()
	-- 		-- require("config.jaq").setup()
	-- 	end,
	-- })
	--
	{
		"ray-x/sad.nvim",
		dependencies = { "ray-x/guihua.lua", build = "cd lua/fzy && make" },
		config = function()
			require("sad").setup({})
		end,
	},
	-- Rust
	{
		"simrat39/rust-tools.nvim",
		dependencies = { "nvim-lua/plenary.nvim", "rust-lang/rust.vim" },
		lazy = true,
		ft = { "rust" },
		-- branch = "modularize_and_inlay_rewrite",
		-- config = function()
		--   require("config.rust").setup()
		-- end,
	},
	{
		"saecki/crates.nvim",
		event = { "BufRead Cargo.toml" },
		dependencies = { { "nvim-lua/plenary.nvim" } },
		config = function()
			-- local null_ls = require "null-ls"
			require("crates").setup({
				null_ls = {
					enabled = true,
					name = "crates.nvim",
				},
			})
		end,
		-- disable = false,
	},

	{
		"danymat/neogen",
		config = function()
			require("neogen").setup({})
		end,
		dependencies = "nvim-treesitter/nvim-treesitter",
		-- Uncomment next line if you want to follow only stable versions
		-- version = "*"
	},
	{
		"windwp/nvim-autopairs",
		lazy = true,
		event = "InsertEnter",
		dependencies = "nvim-treesitter/nvim-treesitter",
		config = function()
			require("nvim-autopairs").setup({})
		end,
	},

	{
		"mfussenegger/nvim-jdtls",
		config = function()
			-- https://github.com/fitrh/init.nvim/blob/main/lua/plugin/jdtls/config.lua
			-- require("plugin.jdtls.config").attach()
		end,
	},
	{
		"stevearc/aerial.nvim",
		config = function()
			require("aerial").setup()
		end,
		cmd = { "AerialToggle" },
	},

	{
		"akinsho/toggleterm.nvim",
		version = "*",
		config = function()
			require("config.toggleterm").setup()
		end,
	},
	{
		"abecodes/tabout.nvim",
		dependencies = "nvim-treesitter/nvim-treesitter",
		-- after = { "nvim-cmp", "nvim-treesitter" },
		config = function()
			require("tabout").setup({
				completion = false,
				ignore_beginning = true,
			})
		end,
	},

	{
		"stevearc/dressing.nvim",
		event = "BufReadPre",
		config = function()
			require("dressing").setup({
				input = { relative = "editor" },
				select = {
					backend = { "telescope", "fzf", "builtin" },
				},
			})
		end,
		-- disable = false,
	},

	-- Auto tag
	{
		"windwp/nvim-ts-autotag",
		lazy = true,
		event = "InsertEnter",
		config = function()
			require("nvim-ts-autotag").setup({ enable = true })
		end,
	},

	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			require("config.treesitter").setup()
		end,
		dependencies = {
			{ "nvim-treesitter/nvim-treesitter-textobjects", event = "BufReadPre" },
			{ "windwp/nvim-ts-autotag", event = "InsertEnter" },
			{ "JoosepAlviste/nvim-ts-context-commentstring", event = "BufReadPre" },
			{ "p00f/nvim-ts-rainbow", event = "BufReadPre", enabled = false },
			{ "RRethy/nvim-treesitter-textsubjects", event = "BufReadPre" },
			{ "nvim-treesitter/playground", cmd = { "TSPlaygroundToggle" } },
			-- {
			--   "lewis6991/spellsitter.nvim",
			--   config = function()
			--     require("spellsitter").setup()
			--   end,
			-- },
			{ "nvim-treesitter/nvim-treesitter-context", event = "BufReadPre", enabled = false },
			{ "mfussenegger/nvim-treehopper", enabled = false },
			{
				"m-demare/hlargs.nvim",
				config = function()
					require("config.hlargs").setup()
				end,
				-- disable = false,
			},
			{
				"AckslD/nvim-FeMaco.lua",
				config = function()
					require("femaco").setup()
				end,
				ft = { "markdown" },
				cmd = { "Femaco" },
				enabled = false,
			},
			-- { "yioneko/nvim-yati", event = "BufReadPre" },
		},
	},

	{
		"ggandor/leap.nvim",
		config = function()
			require("leap").add_default_mappings()
			-- require("config.leap").setup()
		end,
	},

	{
		"TimUntersberger/neogit",
		dependencies = "nvim-lua/plenary.nvim",
		config = function()
			require("neogit").setup()
		end,
	},
	{
		"numToStr/Comment.nvim",
		keys = { "gc", "gcc", "gbc" },
		lazy = false,
		config = function()
			require("Comment").setup()
		end,
	},
	"bronson/vim-trailing-whitespace",
	"osyo-manga/vim-anzu",
	"haya14busa/vim-asterisk",
	"itchyny/lightline.vim",
	"nvim-lua/plenary.nvim",

	{
		"kylechui/nvim-surround",
		version = "*",
		config = function()
			require("nvim-surround").setup({
				keymaps = {
					normal = "gy",
					normal_cur = "gyy",
					normal_line = "gY",
					normal_cur_line = "gYY",
					visual = "gy",
					visual_line = "gY",
					delete = "dy",
					change = "cy",
					change_line = "cY",
				},
			})
		end,
	},

	{
		"glepnir/lspsaga.nvim",
		cmd = { "Lspsaga" },
		config = function()
			require("lspsaga").setup({})
		end,
	},
	{
		"folke/trouble.nvim",
		cmd = { "TroubleToggle", "Trouble" },
		config = function()
			require("trouble").setup({
				use_diagnostic_signs = true,
			})
		end,
	},
	{
		"neovim/nvim-lspconfig",
		config = function()
			require("config.lsp").setup()
		end,
		dependencies = {
			"williamboman/mason.nvim",
			"williamboman/mason-lspconfig.nvim",
			"WhoIsSethDaniel/mason-tool-installer.nvim",
			{ "jayp0521/mason-null-ls.nvim" },
			"ray-x/lsp_signature.nvim",
			"folke/lua-dev.nvim",
			"RRethy/vim-illuminate",
			"nvimtools/none-ls.nvim",
			{ "b0o/schemastore.nvim" },
			{ "jose-elias-alvarez/typescript.nvim" },
			"alpha2phi/nvim-navic",
			config = function()
				require("nvim-navic").setup({})
			end,
			-- {
			-- 	"j-hui/fidget.nvim",
			-- 	tag = "legacy",
			-- 	config = function()
			-- 		require("fidget").setup({
			-- 			ignore = { "code_action", "null-ls" },
			-- 		})
			-- 	end,
			-- },
			{
				"simrat39/inlay-hints.nvim",
				config = function()
					require("inlay-hints").setup()
				end,
			},
		},
	},
	{ "vim-pandoc/vim-pandoc", ft = { "markdown" } },
	{ "tpope/vim-markdown", ft = { "markdown" } },
	{ "junegunn/goyo.vim", ft = { "markdown" } },
	{ "junegunn/limelight.vim", ft = { "markdown" } },
	{ "ledger/vim-ledger", ft = { "ledger" } },

	{
		"jpalardy/vim-slime",
		init = function()
			vim.b["quarto_is_" .. "python" .. "_chunk"] = false
			Quarto_is_in_python_chunk = function()
				require("otter.tools.functions").is_otter_language_context("python")
			end

			vim.cmd([[
                        let g:slime_dispatch_ipython_pause = 100
                        function SlimeOverride_EscapeText_quarto(text)
                          call v:lua.Quarto_is_in_python_chunk()
                          if exists('g:slime_python_ipython') && len(split(a:text,"\n")) > 1 && b:quarto_is_python_chunk
                            return ["%cpaste -q\n", g:slime_dispatch_ipython_pause, a:text, "--", "\n"]
                          else
                            return a:text
                          end
                          endfunction
                          ]])

			local function mark_terminal()
				vim.g.slime_last_channel = vim.b.terminal_job_id
				vim.print(vim.g.slime_last_channel)
			end

			local function set_terminal()
				vim.b.slime_config = { jobid = vim.g.slime_last_channel }
			end

			-- slime, neovvim terminal
			vim.g.slime_target = "neovim"
			vim.g.slime_python_ipython = 1

			require("which-key").register({
				["<leader>cm"] = { mark_terminal, "mark terminal" },
				["<leader>cs"] = { set_terminal, "set terminal" },
			})
		end,
	},
	-- Completion
	{
		"hrsh7th/nvim-cmp",
		event = "InsertEnter",
		lazy = true,
		config = function()
			require("config.cmp").setup()
		end,
		dependencies = {
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-nvim-lua",
			"ray-x/cmp-treesitter",
			"hrsh7th/cmp-cmdline",
			"saadparwaiz1/cmp_luasnip",
			{ "hrsh7th/cmp-nvim-lsp" },
			"hrsh7th/cmp-nvim-lsp-signature-help",
			"lukas-reineke/cmp-rg",
			"davidsierradz/cmp-conventionalcommits",
			{ "onsails/lspkind-nvim" },
			"jmbuhr/otter.nvim",
			-- "hrsh7th/cmp-calc",
			-- "f3fora/cmp-spell",
			-- "hrsh7th/cmp-emoji",
			{
				"L3MON4D3/LuaSnip",
				config = function()
					require("config.snippets").setup()
				end,
				dependencies = { "rafamadriz/friendly-snippets" },
				build = "make install_jsregexp",
			},
			"rafamadriz/friendly-snippets",
			"honza/vim-snippets",
			-- { "tzachar/cmp-tabnine", build = "./install.sh" },
		},
	},

	{
		"michaelb/sniprun",
		build = "bash ./install.sh",
		cmd = { "SnipRun", "SnipInfo", "SnipReset", "SnipReplMemoryClean", "SnipClose", "SnipLive" },
		dependencies = { "folke/which-key.nvim" },
		config = function()
			require("config.sniprun").setup()
		end,
	},

	{
		"rcarriga/nvim-notify",
		event = "BufReadPre",
		config = function()
			require("config.notify").setup()
		end,
		-- disable = false,
	},

	{
		"nvim-telescope/telescope.nvim",
		dependencies = {
			"nvim-lua/popup.nvim",
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter",
			"SalOrak/whaler",

			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
			},
			{
				"nvim-telescope/telescope-frecency.nvim",
				dependencies = "tami5/sqlite.lua",
			},
			{ "nvim-telescope/telescope-smart-history.nvim" },
			{ "cljoly/telescope-repo.nvim" },
			{ "Zane-/cder.nvim" },
		},
		config = function()
			require("config.telescope").setup()
		end,
	},

	{
		"folke/which-key.nvim",
		config = function()
			require("config.whichkey").setup()
		end,
	},
	{
		"nyngwang/suave.lua",
		config = function()
			require("config.suave").setup()
		end,
	},
	{ "mrjones2014/legendary.nvim", version = "v2.2.0" },
	{ "jubnzv/virtual-types.nvim" },
	{
		"ldelossa/litee.nvim",
		event = "VeryLazy",
		opts = {
			notify = { enabled = false },
			panel = {
				orientation = "bottom",
				panel_size = 10,
			},
		},
		config = function(_, opts)
			require("litee.lib").setup(opts)
		end,
	},

	{
		"ldelossa/litee-calltree.nvim",
		dependencies = "ldelossa/litee.nvim",
		event = "VeryLazy",
		opts = {
			on_open = "panel",
			map_resize_keys = false,
		},
		config = function(_, opts)
			require("litee.calltree").setup(opts)
		end,
	},
	-- Debugging
	{
		"mfussenegger/nvim-dap",
		lazy = true,
		event = "BufReadPre",
		keys = { [[<leader>d]] },
		dependencies = {
			"theHamsta/nvim-dap-virtual-text",
			"rcarriga/nvim-dap-ui",
			"mfussenegger/nvim-dap-python",
			"nvim-telescope/telescope-dap.nvim",
			{ "leoluz/nvim-dap-go" },
			{ "jbyuki/one-small-step-for-vimkind" },
			{ "mxsdev/nvim-dap-vscode-js" },
			{
				"microsoft/vscode-js-debug",
				build = "npm install --legacy-peer-deps && npm run compile",
			},
		},
		config = function()
			require("config.dap").setup()
		end,
		-- disable = false,
	},
	{
		"jay-babu/mason-nvim-dap.nvim",
		config = function()
			require("mason-nvim-dap").setup({
				automatic_setup = true,
				ensure_installed = { "stylua", "jq", "node2", "js", "chrome", "firefox", "js-debug-adapter" },
			})
		end,
	},

	{
		"gnikdroy/projections.nvim",
		dependencies = { "nvim-telescope/telescope.nvim" },
		branch = "pre_release",
		config = function()
			require("config.projections").setup()
		end,
	},

	{
		"chentoast/marks.nvim",
		config = function()
			require("config.marks").setup()
		end,
	},
	{
		"bennypowers/nvim-regexplainer",
		config = function()
			require("regexplainer").setup()
		end,
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"MunifTanjim/nui.nvim",
		},
	},
	{
		"kevinhwang91/nvim-hlslens",
		event = "BufReadPre",
		config = function()
			require("config.hslens").setup()
		end,
	},
}, {
	concurrency = 2,
})

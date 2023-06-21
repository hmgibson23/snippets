---@module Plugins
---@author Hugo Gibson

-- vim.cmd([[
--  augroup packer_user_config
--    autocmd!
--    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
--   augroup end
--
-- ])
return require("packer").startup({
	function(use)
		-- Packer can manage itself
		use("wbthomason/packer.nvim")
		use({ "lewis6991/impatient.nvim" })
		use({
			"nvim-tree/nvim-web-devicons",
			module = "nvim-web-devicons",
			config = function()
				require("nvim-web-devicons").setup({ default = true })
			end,
		})

		use({
			"nvim-tree/nvim-tree.lua",
			cmd = { "NvimTreeToggle", "NvimTreeClose" },
			config = function()
				require("config.nvimtree").setup()
			end,
		})

		use({
			"goolord/alpha-nvim",
			config = function()
				require("config.alpha").setup()
			end,
		})

		-- use({
		-- 	"jinh0/eyeliner.nvim",
		-- 	keys = { "F", "f", "T", "t" },
		-- 	config = function()
		-- 		require("eyeliner").setup({
		-- 			highlight_on_key = true,
		-- 			dim = true
		-- 		})
		-- 	end,
		-- })

		use({
			"echasnovski/mini.nvim",
			event = { "BufReadPre" },
			config = function()
				require("mini.align").setup()
				require("mini.test").setup()
				require("mini.doc").setup()
			end,
		})
		use({
			"TaDaa/vimade",
			cmd = { "VimadeToggle", "VimadeEnable", "VimadeDisable" },
			-- disable = true,
			config = function()
				vim.g.vimade.fadelevel = 0.7
				vim.g.vimade.enablesigns = 1
			end,
		})

		use({
			"nvim-neotest/neotest",
			requires = {
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
				{ "nvim-neotest/neotest-vim-test", module = { "neotest-vim-test" } },
				{ "nvim-neotest/neotest-python", module = { "neotest-python" } },
				{ "nvim-neotest/neotest-plenary", module = { "neotest-plenary" } },
				{ "nvim-neotest/neotest-go", module = { "neotest-go" } },
				{ "haydenmeade/neotest-jest", module = { "neotest-jest" } },
				{ "rouge8/neotest-rust", module = { "neotest-rust" } },
			},
			module = { "neotest", "neotest.async" },
			config = function()
				require("config.neotest").setup()
			end,
		})
		use("b0o/schemastore.nvim")
		use({
			"stevearc/overseer.nvim",
			module = { "neotest.consumers.overseer" },
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
		})
		use({
			"hkupty/iron.nvim",
			config = function()
				require("config.iron").setup()
			end,
		})
		use({
			"lewis6991/gitsigns.nvim",
			config = function()
				require("gitsigns").setup()
			end,
		})
		-- use({
		-- 	"is0n/jaq-nvim",
		-- 	config = function()
		-- 		-- require("config.jaq").setup()
		-- 	end,
		-- })
		--
		use({
			"ray-x/sad.nvim",
			requires = { "ray-x/guihua.lua", run = "cd lua/fzy && make" },
			config = function()
				require("sad").setup({})
			end,
		})
		-- Rust
		use({
			"simrat39/rust-tools.nvim",
			requires = { "nvim-lua/plenary.nvim", "rust-lang/rust.vim" },
			opt = true,
			module = "rust-tools",
			ft = { "rust" },
			-- branch = "modularize_and_inlay_rewrite",
			-- config = function()
			--   require("config.rust").setup()
			-- end,
		})
		use({
			"saecki/crates.nvim",
			event = { "BufRead Cargo.toml" },
			requires = { { "nvim-lua/plenary.nvim" } },
			config = function()
				-- local null_ls = require "null-ls"
				require("crates").setup({
					null_ls = {
						enabled = true,
						name = "crates.nvim",
					},
				})
			end,
			disable = false,
		})

		use({
			"danymat/neogen",
			config = function()
				require("neogen").setup({})
			end,
			requires = "nvim-treesitter/nvim-treesitter",
			-- Uncomment next line if you want to follow only stable versions
			-- tag = "*"
		})
		use({
			"windwp/nvim-autopairs",
			opt = true,
			event = "InsertEnter",
			requires = "nvim-treesitter/nvim-treesitter",
			module = { "nvim-autopairs.completion.cmp", "nvim-autopairs" },
			config = function()
				require("nvim-autopairs").setup({})
			end,
		})

		use({ "mfussenegger/nvim-jdtls", ft = { "java" } })
		use({
			"stevearc/aerial.nvim",
			config = function()
				require("aerial").setup()
			end,
			module = { "aerial" },
			cmd = { "AerialToggle" },
		})

		use({
			"akinsho/toggleterm.nvim",
			tag = "*",
			config = function()
				require("toggleterm").setup({
					hide_numbers = true,
					shade_terminals = true,
					winbar = {
						enabled = false,
						name_formatter = function(term)
							return term.name
						end,
					},
				})
			end,
		})
		use({
			"abecodes/tabout.nvim",
			requires = "nvim-treesitter/nvim-treesitter",
			after = { "nvim-cmp", "nvim-treesitter" },
			config = function()
				require("tabout").setup({
					completion = false,
					ignore_beginning = true,
				})
			end,
		})

		use({
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
			disable = false,
		})

		-- Auto tag
		-- use({
		-- 	"windwp/nvim-ts-autotag",
		-- 	opt = true,
		-- 	event = "InsertEnter",
		-- 	config = function()
		-- 		require("nvim-ts-autotag").setup({ enable = true })
		-- 	end,
		-- })

		use({
			"nvim-treesitter/nvim-treesitter",
			run = ":TSUpdate",
			config = function()
				require("config.treesitter").setup()
			end,
			requires = {
				{ "nvim-treesitter/nvim-treesitter-textobjects", event = "BufReadPre" },
				{ "windwp/nvim-ts-autotag", event = "InsertEnter" },
				{ "JoosepAlviste/nvim-ts-context-commentstring", event = "BufReadPre" },
				{ "p00f/nvim-ts-rainbow", event = "BufReadPre", disable = true },
				{ "RRethy/nvim-treesitter-textsubjects", event = "BufReadPre" },
				{ "nvim-treesitter/playground", cmd = { "TSPlaygroundToggle" } },
				-- {
				--   "lewis6991/spellsitter.nvim",
				--   config = function()
				--     require("spellsitter").setup()
				--   end,
				-- },
				{ "nvim-treesitter/nvim-treesitter-context", event = "BufReadPre", disable = true },
				{ "mfussenegger/nvim-treehopper", module = { "tsht" }, disable = true },
				{
					"m-demare/hlargs.nvim",
					config = function()
						require("config.hlargs").setup()
					end,
					disable = false,
				},
				{
					"AckslD/nvim-FeMaco.lua",
					config = function()
						require("femaco").setup()
					end,
					ft = { "markdown" },
					cmd = { "Femaco" },
					module = { "femaco_edit" },
					disable = true,
				},
				-- { "yioneko/nvim-yati", event = "BufReadPre" },
			},
		})

		use({
			"ggandor/leap.nvim",
			config = function()
				require("leap").add_default_mappings()
				-- require("config.leap").setup()
			end,
		})

		use({
			"TimUntersberger/neogit",
			requires = "nvim-lua/plenary.nvim",
			config = function()
				require("neogit").setup()
			end,
		})
		use({
			"numToStr/Comment.nvim",
			keys = { "gc", "gcc", "gbc" },
			config = function()
				require("Comment").setup({
					ignore = "^$",
					pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
				})
			end,
		})
		use("bronson/vim-trailing-whitespace")
		use("osyo-manga/vim-anzu")
		use("haya14busa/vim-asterisk")
		use("itchyny/lightline.vim")
		use("nvim-lua/plenary.nvim")

		use({
			"kylechui/nvim-surround",
			tag = "*", -- Use for stability; omit to use `main` branch for the latest features
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
		})

		use({
			"glepnir/lspsaga.nvim",
			cmd = { "Lspsaga" },
			config = function()
				require("lspsaga").init_lsp_saga()
			end,
		})
		use({
			"folke/trouble.nvim",
			cmd = { "TroubleToggle", "Trouble" },
			module = { "trouble.providers.telescope" },
			config = function()
				require("trouble").setup({
					use_diagnostic_signs = true,
				})
			end,
		})
		use({
			"neovim/nvim-lspconfig",
			config = function()
				require("config.lsp").setup()
			end,
			requires = {
				"williamboman/mason.nvim",
				"williamboman/mason-lspconfig.nvim",
				"WhoIsSethDaniel/mason-tool-installer.nvim",
				{ "jayp0521/mason-null-ls.nvim" },
				"ray-x/lsp_signature.nvim",
				"folke/lua-dev.nvim",
				"RRethy/vim-illuminate",
				"jose-elias-alvarez/null-ls.nvim",
				{ "b0o/schemastore.nvim", module = { "schemastore" } },
				{ "jose-elias-alvarez/typescript.nvim", module = { "typescript" } },
				"alpha2phi/nvim-navic",
				config = function()
					require("nvim-navic").setup({})
				end,
				module = { "nvim-navic" },
				{
					"j-hui/fidget.nvim",
					tag = "legacy",
					config = function()
						require("fidget").setup({})
					end,
				},
				{
					"simrat39/inlay-hints.nvim",
					config = function()
						require("inlay-hints").setup()
					end,
				},
			},
		})
		use({ "vim-pandoc/vim-pandoc", ft = { "markdown" } })
		use({ "tpope/vim-markdown", ft = { "markdown" } })
		use({ "junegunn/goyo.vim", ft = { "markdown" } })
		use({ "junegunn/limelight.vim", ft = { "markdown" } })
		use({ "ledger/vim-ledger", ft = { "ledger" } })

		-- Completion
		use({
			"hrsh7th/nvim-cmp",
			event = "InsertEnter",
			opt = true,
			config = function()
				require("config.cmp").setup()
			end,
			wants = { "LuaSnip", "lspkind-nvim" },
			requires = {
				"hrsh7th/cmp-buffer",
				"hrsh7th/cmp-path",
				"hrsh7th/cmp-nvim-lua",
				"ray-x/cmp-treesitter",
				"hrsh7th/cmp-cmdline",
				"saadparwaiz1/cmp_luasnip",
				{ "hrsh7th/cmp-nvim-lsp", module = { "cmp_nvim_lsp" } },
				"hrsh7th/cmp-nvim-lsp-signature-help",
				"lukas-reineke/cmp-rg",
				"davidsierradz/cmp-conventionalcommits",
				{ "onsails/lspkind-nvim", module = { "lspkind" } },
				-- "hrsh7th/cmp-calc",
				-- "f3fora/cmp-spell",
				-- "hrsh7th/cmp-emoji",
				{
					"L3MON4D3/LuaSnip",
					wants = { "friendly-snippets", "vim-snippets" },
					config = function()
						require("config.snippets").setup()
					end,
					run = "make install_jsregexp",
				},
				"rafamadriz/friendly-snippets",
				"honza/vim-snippets",
				-- { "tzachar/cmp-tabnine", run = "./install.sh" },
			},
		})

		use({
			"michaelb/sniprun",
			run = "bash ./install.sh",
			cmd = { "SnipRun", "SnipInfo", "SnipReset", "SnipReplMemoryClean", "SnipClose", "SnipLive" },
			module = { "sniprun", "sniprun.api" },
			requires = { "folke/which-key.nvim" },
			config = function()
				require("config.sniprun").setup()
			end,
		})

		use({
			"rcarriga/nvim-notify",
			event = "BufReadPre",
			config = function()
				require("config.notify").setup()
			end,
			disable = false,
		})

		use({
			"nvim-telescope/telescope.nvim",
			requires = {
				"nvim-lua/popup.nvim",
				"nvim-lua/plenary.nvim",
				"nvim-treesitter/nvim-treesitter",
				{
					"nvim-telescope/telescope-fzf-native.nvim",
					run = "make",
				},
				{
					"nvim-telescope/telescope-frecency.nvim",
					requires = "tami5/sqlite.lua",
				},
				{ "nvim-telescope/telescope-smart-history.nvim" },
				{ "cljoly/telescope-repo.nvim" },
				{ "nvim-telescope/telescope-project.nvim" },
				{ "Zane-/cder.nvim" },
				{
					"ahmedkhalf/project.nvim",
					config = function()
						require("project_nvim").setup({
							sync_root_with_cwd = true,
							respect_buf_cwd = true,
							update_focused_file = {
								enable = true,
								update_root = true,
							},
							ignore_lsp = { "null-ls" },
						})
					end,
				},
			},
			config = function()
				require("config.telescope").setup()
			end,
		})

		use({
			"folke/which-key.nvim",
			config = function()
				require("config.whichkey").setup()
			end,
		})
		use({ "mrjones2014/legendary.nvim", tag = "v2.2.0" })
		-- Debugging
		use({
			"mfussenegger/nvim-dap",
			opt = true,
			event = "BufReadPre",
			keys = { [[<leader>d]] },
			module = { "dap" },
			wants = { "nvim-dap-virtual-text", "nvim-dap-ui", "nvim-dap-python", "which-key.nvim" },
			requires = {
				"theHamsta/nvim-dap-virtual-text",
				"rcarriga/nvim-dap-ui",
				"mfussenegger/nvim-dap-python",
				"nvim-telescope/telescope-dap.nvim",
				{ "leoluz/nvim-dap-go", module = "dap-go" },
				{ "jbyuki/one-small-step-for-vimkind", module = "osv" },
				{ "mxsdev/nvim-dap-vscode-js", module = { "dap-vscode-js" } },
				{
					"microsoft/vscode-js-debug",
					run = "npm install --legacy-peer-deps && npm run compile",
				},
			},
			config = function()
				require("config.dap").setup()
			end,
			disable = false,
		})
		use({
			"jay-babu/mason-nvim-dap.nvim",
			requires = {
				"williamboman/mason.nvim",
				"mfussenegger/nvim-dap",
			},
			config = function()
				require("mason-nvim-dap").setup({
					automatic_setup = true,
					ensure_installed = { "stylua", "jq", "node2", "js", "chrome", "firefox" },
				})
			end,
		})

		use({
			"gnikdroy/projections.nvim",
			requires = { "nvim-telescope/telescope.nvim" },
			config = function()
				require("config.projections").setup()
			end,
		})

		use({
			"chentoast/marks.nvim",
			config = function()
				require("config.marks").setup()
			end,
		})
		use({
			"kevinhwang91/nvim-hlslens",
			config = function()
				require("config.hslens").setup()
			end,
		})
	end,
	config = {
		clone_timeout = 9999,
	},
})

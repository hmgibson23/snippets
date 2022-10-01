return require("packer").startup(function()
	-- Packer can manage itself
	use("wbthomason/packer.nvim")
	use("b0o/schemastore.nvim")
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
		wants = "nvim-treesitter",
		module = { "nvim-autopairs.completion.cmp", "nvim-autopairs" },
		config = function()
			require("nvim-autopairs").setup({})
		end,
	})

	use("mfussenegger/nvim-jdtls")
	use({
		"stevearc/aerial.nvim",
		config = function()
			require("aerial").setup()
		end,
		module = { "aerial" },
		cmd = { "AerialToggle" },
	})
	use({
		"abecodes/tabout.nvim",
		wants = { "nvim-treesitter" },
		after = { "nvim-cmp" },
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

	use({
		"nvim-treesitter/nvim-treesitter",
		opt = true,
		event = "BufReadPre",
		run = ":TSUpdate",
		config = function()
			require("config.treesitter").setup()
		end,
		requires = {
			{ "nvim-treesitter/nvim-treesitter-textobjects", event = "BufReadPre" },
			{ "windwp/nvim-ts-autotag", event = "InsertEnter" },
			{ "JoosepAlviste/nvim-ts-context-commentstring", event = "BufReadPre" },
			{ "nvim-treesitter/nvim-treesitter-context", event = "BufReadPre" },
			-- { "RRethy/nvim-treesitter-textsubjects", event = "BufReadPre" },
			{ "p00f/nvim-ts-rainbow", event = "BufReadPre" },
		},
	})

	use("airblade/vim-gitgutter")
	use("bronson/vim-trailing-whitespace")
	use("editorconfig/editorconfig-vim")
	use("mhinz/vim-startify")
	use("easymotion/vim-easymotion")
	use("wellle/targets.vim")
	use("haya14busa/incsearch.vim")
	use("jiangmiao/auto-pairs")
	use("vim-scripts/grep.vim")
	use("vim-scripts/mru.vim")
	use("osyo-manga/vim-anzu")
	use("sheerun/vim-polyglot")
	use("tpope/vim-commentary")
	use("tpope/vim-eunuch")
	use("tpope/vim-sleuth")
	use("tpope/vim-surround")
	use("tpope/vim-fugitive")
	use("itchyny/lightline.vim")
	use("puremourning/vimspector")
	use("nvim-lua/plenary.nvim")
	use("janko-m/vim-test")

	use({
		"neovim/nvim-lspconfig",
		event = "VimEnter",
		-- event = { "BufReadPre" },
		wants = {
			"lsp_signature.nvim",
			"lua-dev.nvim",
			"vim-illuminate",
			"null-ls.nvim",
			"schemastore.nvim",
			"mason.nvim",
			"cmp-nvim-lsp",
			"nvim-navic",
			"mason-lspconfig.nvim",
			"mason-tool-installer.nvim",
			"inlay-hints.nvim",
			-- "nvim-lsp-ts-utils",
			"typescript.nvim",
		}, -- for coq.nvim
		config = function()
			require("config.lsp").setup()
		end,
		requires = {
			"williamboman/mason.nvim",
			"williamboman/mason-lspconfig.nvim",
			"WhoIsSethDaniel/mason-tool-installer.nvim",
			"ray-x/lsp_signature.nvim",
			"folke/lua-dev.nvim",
			"RRethy/vim-illuminate",
			"jose-elias-alvarez/null-ls.nvim",
			"b0o/schemastore.nvim",
			"jose-elias-alvarez/typescript.nvim",
			"alpha2phi/nvim-navic",
			config = function()
				require("nvim-navic").setup({})
			end,
			module = { "nvim-navic" },
			{
				"j-hui/fidget.nvim",
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
		"hrsh7th/cmp-nvim-lsp",
	})

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
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-nvim-lsp-signature-help",
			"lukas-reineke/cmp-rg",
			"davidsierradz/cmp-conventionalcommits",
			"onsails/lspkind-nvim",
			-- "hrsh7th/cmp-calc",
			-- "f3fora/cmp-spell",
			-- "hrsh7th/cmp-emoji",
			{
				"L3MON4D3/LuaSnip",
				wants = { "friendly-snippets", "vim-snippets" },
				-- config = function()
				--   require("config.snip").setup()
				-- end,
			},
			"rafamadriz/friendly-snippets",
			"honza/vim-snippets",
			-- { "tzachar/cmp-tabnine", run = "./install.sh" },
		},
	})

	use({
		"nvim-telescope/telescope.nvim",
		requires = { { "nvim-lua/plenary.nvim" } },
	})
	use({
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup({
				-- your configuration comes here
				-- or leave it empty to use the default settings
				-- refer to the configuration section below
			})
		end,
	})
	use({ "mrjones2014/legendary.nvim" })
	-- Debugging
	use({
		"mfussenegger/nvim-dap",
		opt = true,
		-- event = "BufReadPre",
		keys = { [[<leader>d]] },
		module = { "dap" },
		wants = { "nvim-dap-virtual-text", "nvim-dap-ui", "nvim-dap-python", "which-key.nvim" },
		requires = {
			-- "alpha2phi/DAPInstall.nvim",
			-- { "Pocco81/dap-buddy.nvim", branch = "dev" },
			"theHamsta/nvim-dap-virtual-text",
			"rcarriga/nvim-dap-ui",
			"mfussenegger/nvim-dap-python",
			"nvim-telescope/telescope-dap.nvim",
			{ "leoluz/nvim-dap-go", module = "dap-go" },
			{ "jbyuki/one-small-step-for-vimkind", module = "osv" },
		},
		config = function()
			require("config.dap").setup()
		end,
		disable = false,
	})
end)
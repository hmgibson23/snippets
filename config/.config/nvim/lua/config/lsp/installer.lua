local M = {}

function M.setup(servers, server_options)
	local lspconfig = require("lspconfig")
  local icons = require "config.icons"

	require("mason").setup({
		ui = {
			icons = {
				package_installed = icons.lsp.server_installed,
				package_pending = icons.lsp.server_pending,
				package_uninstalled = icons.lsp.server_uninstalled,
			},
		},
	})

	require("mason-null-ls").setup({
		automatic_setup = true,
	})
	-- require("mason-null-ls").setup_handlers()

	require("mason-tool-installer").setup({
		ensure_installed = { "codelldb", "stylua", "shfmt", "shellcheck", "prettierd", "terraform-ls", "prettierd" },
		auto_update = true,
		run_on_start = true,
	})

	require("mason-lspconfig").setup({
		ensure_installed = vim.tbl_keys(servers),
		automatic_installation = false,
	})

	-- Package installation folder
	local install_root_dir = vim.fn.stdpath("data") .. "/mason"

	require("mason-lspconfig").setup_handlers({
		function(server_name)
			local opts = vim.tbl_deep_extend("force", server_options, servers[server_name] or {})
			lspconfig[server_name].setup(opts)
		end,
		["jdtls"] = function()
			-- print "jdtls is handled by nvim-jdtls"
		end,
		["awk_ls"] = function() end,
		["lua_ls"] = function()
			local opts = vim.tbl_deep_extend("force", server_options, servers["lua_ls"] or {})
			require("neodev").setup({ lspconfig = opts })
			lspconfig.lua_ls.setup({})
		end,
		["terraformls"] = function()
			vim.api.nvim_create_autocmd({ "BufWritePre" }, {
				pattern = { "*.tf", "*.tfvars" },
				callback = function()
					vim.lsp.buf.format()
				end,
			})
		end,
		["rust_analyzer"] = function()
			local opts = vim.tbl_deep_extend("force", server_options, servers["rust_analyzer"] or {})

			opts.checkOnSave = {
				extraArgs = { "--target-dir", "/tmp/rust-analyzer-check" },
			}
			-- DAP settings - https://github.com/simrat39/rust-tools.nvim#a-better-debugging-experience
			local extension_path = install_root_dir .. "/packages/codelldb/extension/"
			local codelldb_path = extension_path .. "adapter/codelldb"
			local liblldb_path = extension_path .. "lldb/lib/liblldb.so"
			local ih = require("inlay-hints")
			require("rust-tools").setup({
				tools = {
					-- executor = require("rust-tools/executors").toggleterm,
					hover_actions = { border = "solid" },
					on_initialized = function()
						vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter", "CursorHold", "InsertLeave" }, {
							pattern = { "*.rs" },
							callback = function()
								vim.lsp.codelens.refresh()
							end,
						})
						ih.set_all()
					end,
					inlay_hints = {
						auto = false,
					},
				},
				server = opts,
				dap = {
					adapter = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path),
				},
			})
		end,
		["tsserver"] = function()
			local opts = vim.tbl_deep_extend("force", server_options, servers["tsserver"] or {})
			require("typescript").setup({
				disable_commands = false,
				debug = false,
				server = opts,
			})
		end,
	})
end

return M

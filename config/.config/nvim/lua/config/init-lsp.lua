local M = {}

local servers = {
	gopls = {},
	html = {},
	jsonls = {
		settings = {
			json = {
				schemas = require("schemastore").json.schemas(),
			},
		},
	},
	pyright = {},
	rust_analyzer = {
		settings = {
			["rust-analyzer"] = {
				cargo = { allFeatures = true },
				checkOnSave = {
					command = "clippy",
					extraArgs = { "--no-deps" },
				},
			},
		},
	},
	lua_ls = {
		settings = {
			Lua = {
				runtime = {
					-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
					version = "LuaJIT",
					-- Setup your lua path
					path = vim.split(package.path, ";"),
				},
				diagnostics = {
					-- Get the language server to recognize the `vim` global
					globals = { "vim", "PLUGINS" },
					disable = { "lowercase-global" },
				},
				workspace = {
					-- Make the server aware of Neovim runtime files
					library = {
						[vim.fn.expand("$VIMRUNTIME/lua")] = true,
						[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
					},
					maxPreload = 2000,
					preloadFileSize = 50000,
				},
				completion = { callSnippet = "Both" },
				telemetry = { enable = false },
			},
		},
	},
	tsserver = { disable_formatting = true },
	vimls = {},
	solang = {},
	yamlls = {},
	jdtls = {},
	dockerls = {},
}

function M.on_attach(client, bufnr)
	local caps = client.server_capabilities
	-- Enable completion triggered by <C-X><C-O>
	-- See `:help omnifunc` and `:help ins-completion` for more information.
	vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

	-- Use LSP as the handler for formatexpr.
	-- See `:help formatexpr` for more information.
	vim.api.nvim_buf_set_option(0, "formatexpr", "v:lua.vim.lsp.formatexpr()")

	-- Configure key mappings
	local bufopts = { noremap = true, silent = true, buffer = bufnr }
	vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
	vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
	vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
	vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
	vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
	vim.keymap.set("n", "<space>wa", vim.lsp.buf.add_workspace_folder, bufopts)
	vim.keymap.set("n", "<space>wr", vim.lsp.buf.remove_workspace_folder, bufopts)
	vim.keymap.set("n", "<space>wl", function()
		print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
	end, bufopts)
	vim.keymap.set("n", "<space>D", vim.lsp.buf.type_definition, bufopts)
	vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, bufopts)
	vim.keymap.set("n", "<space>ca", vim.lsp.buf.code_action, bufopts)
	vim.keymap.set("n", "gr", vim.lsp.buf.references, bufopts)
	vim.keymap.set("n", "<space>f", vim.lsp.buf.format, bufopts)
	-- Configure highlighting
	-- require("config.lsp.highlighter").setup(client)

	-- Configure formatting
	require("config.lsp.null-ls.formatters").setup(client, bufnr)

	-- Configure for Typescript
	if client.name ~= "null-ls" then
		-- aerial.nvim
		require("aerial").setup()

		-- inlay-hints
		-- local ih = require "inlay-hints"
		-- ih.on_attach(client, bufnr)

		-- -- semantic highlighting
		-- if caps.semanticTokensProvider and caps.semanticTokensProvider.full then
		--   local augroup = vim.api.nvim_create_augroup("SemanticTokens", {})
		--   vim.api.nvim_create_autocmd("TextChanged", {
		--     group = augroup,
		--     buffer = bufnr,
		--     callback = function()
		--       vim.lsp.buf.semantic_tokens_full()
		--     end,
		--   })
		--   -- fire it first time on load as well
		--   vim.lsp.buf.semantic_tokens_full()
		-- end
	end
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

local opts = {
	on_attach = M.on_attach,
	capabilities = M.capabilities,
	flags = {
		debounce_text_changes = 150,
	},
}

-- Setup LSP handlers
-- require("config.lsp.handlers").setup()

-- null-ls
-- require("config.lsp.null-ls").setup(opts)

local diagnostics_active = true

function M.toggle_diagnostics()
	diagnostics_active = not diagnostics_active
	if diagnostics_active then
		vim.diagnostic.show()
	else
		vim.diagnostic.hide()
	end
end

function M.remove_unused_imports()
	vim.diagnostic.setqflist({ severity = vim.diagnostic.severity.WARN })
	vim.cmd("packadd cfilter")
	vim.cmd("Cfilter /main/")
	vim.cmd("Cfilter /The import/")
	vim.cmd("cdo normal dd")
	vim.cmd("cclose")
	vim.cmd("wa")
end

function M.setup()
	require("config.lsp.installer").setup(servers, opts)
end

return M

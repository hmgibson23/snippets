local M = {}

local whichkey = require("which-key")
local legendary = require("legendary")
local l = require("legendary.integrations.which-key")

-- local keymap = vim.api.nvim_set_keymap
-- local buf_keymap = vim.api.nvim_buf_set_keymap
local keymap = vim.keymap.set

local function keymappings(client, bufnr)
	local opts = { noremap = true, silent = true }

	-- Key mappings
	-- buf_keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
	-- vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = 0 })
	keymap("n", "T", vim.lsp.buf.hover, { buffer = bufnr })

	keymap("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
	keymap("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
	keymap("n", "[e", "<cmd>lua vim.diagnostic.goto_prev({severity = vim.diagnostic.severity.ERROR})<CR>", opts)
	keymap("n", "]e", "<cmd>lua vim.diagnostic.goto_next({severity = vim.diagnostic.severity.ERROR})<CR>", opts)

	-- Whichkey
	local keymap_l = {
		l = {
			name = "LSP",
			R = { "<cmd>Trouble lsp_references<cr>", "Trouble References" },
      D = { "<cmd>DocsViewToggle<CR>", "Toggle Docs View"},
			a = { "<cmd>Lspsaga code_action<CR>", "Code Action" },
			T = { "<cmd>Lspsaga term_toggle<CR>", "Term toggle" },
			F = { "<cmd>Lspsaga hover_doc<CR>", "Hover doc" },
			O = { "<cmd>Outline<CR>", "Outline" },
			gd = { "<cmd>Glance definitions<CR>", "Glance Definitions" },
			gr = { "<cmd>Glance references<CR>", "Glance References" },
			gy = { "<cmd>Glance type_definitions<CR>", "Glance Types" },
			gm = { "<cmd>Glance implementations<CR>", "Glance Implementations" },
			d = { "<cmd>lua require('telescope.builtin').diagnostics()<CR>", "Diagnostics" },
			f = { "<cmd>Lspsaga finder<CR>", "Finder" },
			i = { "<cmd>LspInfo<CR>", "Lsp Info" },
			n = { "<cmd>Lspsaga rename<CR>", "Rename" },
			o = { "<cmd>Lspsaga outgoing_calls<CR>", "Outgoing Class" },
			r = { "<cmd>lua require('telescope.builtin').lsp_references()<CR>", "References" },
			s = { "<cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>", "Document Symbols" },
			t = { "<cmd>TroubleToggle document_diagnostics<CR>", "Trouble" },
			L = { "<cmd>lua vim.lsp.codelens.refresh()<CR>", "Refresh CodeLens" },
			l = { "<cmd>lua vim.lsp.codelens.run()<CR>", "Run CodeLens" },
			D = { "<cmd>lua require('config.lsp').toggle_diagnostics()<CR>", "Toggle Inline Diagnostics" },
			e = { "<cmd>lua require('aerial').toggle()<CR>", "Toggle Aerial" },
		},
	}
	if client.server_capabilities.documentFormattingProvider then
		keymap_l.l.F = { "<cmd>lua vim.lsp.buf.format({async = true})<CR>", "Format Document" }
	end

	local keymap_g = {
		name = "Goto",
		d = { "<Cmd>lua vim.lsp.buf.definition()<CR>", "Definition" },
		-- d = { "<cmd>lua require('goto-preview').goto_preview_definition()<CR>", "Definition" },
		D = { "<Cmd>lua vim.lsp.buf.declaration()<CR>", "Declaration" },
		h = { "<cmd>lua vim.lsp.buf.signature_help()<CR>", "Signature Help" },
		I = { "<cmd>Telescope lsp_implementations<CR>", "Goto Implementation" },
		b = { "<cmd>lua vim.lsp.buf.type_definition()<CR>", "Goto Type Definition" },
		-- b = { "<cmd>lua require('goto-preview').goto_preview_type_definition()<CR>", "Goto Type Definition" },
	}

	local keymap_v_l = {
		l = {
			name = "LSP",
			a = { "<cmd>'<,'>lua vim.lsp.buf.range_code_action()<CR>", "Code Action" },
		},
	}

	local o = { buffer = bufnr, prefix = "<leader>" }
	whichkey.register(keymap_l, o)
	l.bind_whichkey(keymap_l, o, false)

	o = { mode = "v", buffer = bufnr, prefix = "<leader>" }
	whichkey.register(keymap_v_l, o)
	l.bind_whichkey(keymap_v_l, o, false)

	o = { buffer = bufnr, prefix = "g" }
	whichkey.register(keymap_g, o)
	l.bind_whichkey(keymap_g, o, false)
end

function M.setup(client, bufnr)
	keymappings(client, bufnr)
end

return M

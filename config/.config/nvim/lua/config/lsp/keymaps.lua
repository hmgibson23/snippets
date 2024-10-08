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
		{ "<leader>l", group = "Lsp" },
		{ "<leader>lR", "<cmd>Trouble lsp_references<cr>", desc = "Trouble References" },
		{ "<leader>lw", "<cmd>DocsViewToggle<CR>", desc = "Toggle Docs View" },
		{ "<leader>la", "<cmd>Lspsaga code_action<CR>", desc = "Code Action" },
		{ "<leader>lT", "<cmd>Lspsaga term_toggle<CR>", desc = "Term toggle" },
		{ "<leader>lF", "<cmd>Lspsaga hover_doc<CR>", desc = "Hover doc" },
		{ "<leader>lO", "<cmd>Outline<CR>", desc = "Outline" },
		{ "<leader>lc", "<cmd>lua vim.lsp.codelens.run()", desc = "CodeLens" },
		{ "<leader>lgd", "<cmd>Glance definitions<CR>", desc = "Glance Definitions" },
		{ "<leader>lgr", "<cmd>Glance references<CR>", desc = "Glance References" },
		{ "<leader>lgy", "<cmd>Glance type_definitions<CR>", desc = "Glance Types" },
		{ "<leader>lgm", "<cmd>Glance implementations<CR>", desc = "Glance Implementations" },
		{ "<leader>ld", "<cmd>lua require('telescope.builtin').diagnostics()<CR>", desc = "Diagnostics" },
		{ "<leader>lf", "<cmd>Lspsaga finder<CR>", desc = "Finder" },
		{ "<leader>li", "<cmd>LspInfo<CR>", desc = "Lsp Info" },
		{ "<leader>ln", "<cmd>Lspsaga rename<CR>", desc = "Rename" },
		{ "<leader>lo", "<cmd>Lspsaga outgoing_calls<CR>", desc = "Outgoing Calls" },
		{ "<leader>lr", "<cmd>lua require('telescope.builtin').lsp_references()<CR>", desc = "References" },
		{ "<leader>ls", "<cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>", desc = "Document Symbols" },
		{ "<leader>lt", "<cmd>TroubleToggle document_diagnostics<CR>", desc = "Trouble" },
		{ "<leader>lL", "<cmd>lua vim.lsp.codelens.refresh()<CR>", desc = "Refresh CodeLens" },
		{ "<leader>ll", "<cmd>lua vim.lsp.codelens.run()<CR>", desc = "Run CodeLens" },
		{ "<leader>lD", "<cmd>lua require('config.lsp').toggle_diagnostics()<CR>", desc = "Toggle Inline Diagnostics" },
		{ "<leader>le", "<cmd>lua require('aerial').toggle()<CR>", desc = "Toggle Aerial" },
		{ "<leader>lb", name = "Boo" },
		{ "<leader>lbo", "<cmd>lua require('boo').boo()<CR>", desc = "Show boo" },
		{ "<leader>lbh", "<cmd>lua require('boo').close()<CR>", desc = "Close boo" },
	}

	if client.server_capabilities.documentFormattingProvider then
		table.insert(
			keymap_l,
			{ "<leader>lF", "<cmd>lua vim.lsp.buf.format({async = true})<CR>", desc = "Format Document" }
		)
	end

	local keymap_g = {
		{ "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", desc = "Definition" },
		{ "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", desc = "Declaration" },
		{ "gh", "<cmd>lua vim.lsp.buf.signature_help()<CR>", desc = "Signature Help" },
		{ "gI", "<cmd>Telescope lsp_implementations<CR>", desc = "Goto Implementation" },
		{ "gb", "<cmd>lua vim.lsp.buf.type_definition()<CR>", desc = "Goto Type Definition" },
	}

	local keymap_v_l = {
		{ "<leader>l", mode = "v" },
		{ "<leader>la", "<cmd>'<,'>lua vim.lsp.buf.range_code_action()<CR>", desc = "Code Action", mode = "v" },
	}

	local o = { buffer = bufnr, prefix = "<leader>" }
	whichkey.add(keymap_l)
	l.bind_whichkey(keymap_l, o, false)

	o = { mode = "v", buffer = bufnr, prefix = "<leader>" }
	whichkey.add(keymap_v_l)
	l.bind_whichkey(keymap_v_l, o, false)

	o = { buffer = bufnr, prefix = "g" }
	whichkey.add(keymap_g)
	l.bind_whichkey(keymap_g, o, false)
end

function M.setup(client, bufnr)
	keymappings(client, bufnr)
end

return M

local M = {}

local nls_utils = require("config.lsp.null-ls.utils")
local nls_sources = require("null-ls.sources")
local api = vim.api

local method = require("null-ls").methods.FORMATTING

M.autoformat = true

function M.toggle()
	M.autoformat = not M.autoformat
end

function M.format()
	if M.autoformat then
		local view = vim.fn.winsaveview()
		vim.lsp.buf.format({
			async = true,
			filter = function(client)
				return client.name ~= "ts_ls"
					and client.name ~= "jsonls"
					and client.name ~= "tsserver"
					and client.name ~= "html"
					and client.name ~= "lua_ls"
					and client.name ~= "jdt.ls"
					and client.name ~= ""
				-- and client.name ~= "kotlin_language_server"
			end,
		})
		vim.fn.winrestview(view)
	end
end

function M.setup(client, bufnr)
	local filetype = api.nvim_buf_get_option(bufnr, "filetype")

	local enable = false
	if M.has_formatter(filetype) then
		enable = client.name == "null-ls"
	else
		enable = not (client.name == "null-ls")
	end

	if not enable then
		return
	end
	if client.name == "clangd" then
		client.resolved_capabilities.document_formatting = false
	end

	client.server_capabilities.documentFormattingProvder = enable
	client.server_capabilities.documentRangeFormattingProvider = enable
	if client.server_capabilities.documentFormattingProvider then
		local lsp_format_grp = api.nvim_create_augroup("LspFormat", { clear = true })
		api.nvim_create_autocmd("BufWritePre", {
			callback = function()
				vim.schedule(M.format)
			end,
			group = lsp_format_grp,
			buffer = bufnr,
		})
	end
end

function M.has_formatter(filetype)
	local available = nls_sources.get_available(filetype, method)
	return #available > 0
end

function M.list_registered(filetype)
	local registered_providers = nls_utils.list_registered_providers_names(filetype)
	return registered_providers[method] or {}
end

function M.list_supported(filetype)
	local supported_formatters = nls_sources.get_supported(filetype, "formatting")
	table.sort(supported_formatters)
	return supported_formatters
end

return M

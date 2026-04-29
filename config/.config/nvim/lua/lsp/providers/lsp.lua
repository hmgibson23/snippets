-- ABOUTME: LSP provider implementation for vim.lsp diagnostics and code actions
-- ABOUTME: Wraps native Neovim LSP functionality for the provider interface

local Provider = require('lsp.providers')

local LspProvider = Provider:new()

-- Get diagnostics for buffer using vim.diagnostic
function LspProvider:get_diagnostics(bufnr)
  return vim.diagnostic.get(bufnr)
end

-- Get code actions for buffer and range using vim.lsp
function LspProvider:get_code_actions(bufnr, range)
  local params = vim.lsp.util.make_range_params()
  params.context = {
    diagnostics = vim.diagnostic.get(bufnr, range and { lnum = range[1] } or nil),
    only = nil,
  }

  local results = {}
  vim.lsp.buf_request_sync(bufnr, 'textDocument/codeAction', params, 1000)
  
  -- Note: This is a simplified implementation
  -- In practice we'll need to handle async results properly
  return results
end

-- Get Treesitter node for scope
-- LSP provider delegates to Treesitter for scope detection
function LspProvider:get_scope_node(bufnr, pos, scope_type)
  -- This will be delegated to TreesitterProvider or a scope utility
  -- For now, return nil (not implemented by LSP provider directly)
  return nil
end

-- Check if diagnostic can be suppressed
-- LSP diagnostics can be suppressed if they have a source
function LspProvider:can_suppress(diagnostic)
  return diagnostic.source ~= nil
end

-- Suppress diagnostic at specified scope
-- Delegates to language-specific adapters
function LspProvider:suppress(diagnostic, scope)
  -- This will delegate to language adapters in the diagnostics feature
  -- For now, return false (not implemented yet)
  return false
end

-- Check if provider is available for buffer
-- Available if at least one LSP client is attached
function LspProvider:is_available(bufnr)
  local clients = vim.lsp.get_clients({ bufnr = bufnr })
  return #clients > 0
end

return LspProvider

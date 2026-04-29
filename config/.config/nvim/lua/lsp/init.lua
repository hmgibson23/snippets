-- ABOUTME: Main entry point for LSP enhancements system
-- ABOUTME: Provides setup function and coordinates all features

local M = {}

-- Submodules
M.config = require('lsp.config')
M.capabilities = require('lsp.capabilities')

-- Track if we've been initialized
local _initialized = false

-- Called when LSP client attaches to buffer
function M.on_attach(client, bufnr)
  -- Features will register their own on_attach handlers
  -- This is just a coordination point
  
  local diagnostics = require('lsp.features.diagnostics')
  if diagnostics.on_attach then
    diagnostics.on_attach(client, bufnr)
  end

  local actions = require('lsp.features.actions')
  if actions.on_attach then
    actions.on_attach(client, bufnr)
  end
end

-- Setup the enhancement system
function M.setup(opts)
  if _initialized then
    vim.notify('LSP enhancements already initialized', vim.log.levels.WARN)
    return
  end

  -- Merge user config with defaults
  M.config.merge(opts or {})
  local config = M.config.get()

  -- Setup features
  if config.diagnostics then
    local ok, diagnostics = pcall(require, 'lsp.features.diagnostics')
    if ok then
      diagnostics.setup(config.diagnostics)
    end
  end

  if config.actions then
    local ok, actions = pcall(require, 'lsp.features.actions')
    if ok then
      actions.setup(config.actions)
    end
  end

  _initialized = true
end

-- Get enhanced LSP capabilities
function M.get_capabilities()
  return M.capabilities.build()
end

-- Check if a client supports a feature
function M.supports_feature(client, feature_name)
  return M.capabilities.supports_feature(client, feature_name)
end

return M

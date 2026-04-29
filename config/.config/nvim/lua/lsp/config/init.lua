-- ABOUTME: Configuration loader and merger for LSP enhancements
-- ABOUTME: Handles loading defaults, language-specific configs, and user overrides

local M = {}

-- Cached configuration
M._config = nil

-- Load default configuration
function M.load_defaults()
  return require('lsp.config.defaults')
end

-- Load language-specific configuration
function M.load_language_config(filetype)
  local ok, lang_config = pcall(require, 'lsp.config.languages.' .. filetype)
  if ok then
    return lang_config
  end
  return {}
end

-- Merge configurations with priority: user > language > defaults
function M.merge(user_config)
  local defaults = M.load_defaults()
  local filetype = vim.bo.filetype
  local lang_config = M.load_language_config(filetype)

  -- Deep merge: defaults <- lang_config <- user_config
  M._config = vim.tbl_deep_extend('force', defaults, lang_config, user_config or {})
  return M._config
end

-- Get current configuration
function M.get()
  if not M._config then
    M._config = M.load_defaults()
  end
  return M._config
end

-- Get configuration for specific feature
function M.get_feature(feature_name)
  local config = M.get()
  return config[feature_name] or {}
end

-- Reset configuration to defaults
function M.reset()
  M._config = nil
end

return M

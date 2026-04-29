-- ABOUTME: None-ls (null-ls) provider for external tool diagnostics and code actions
-- ABOUTME: Wraps none-ls functionality to integrate with provider abstraction

local Provider = require('lsp.providers')

local NoneLsProvider = Provider:new()

-- Check if none-ls is loaded
local function is_none_ls_loaded()
  return package.loaded['null-ls'] ~= nil
end

-- Get list of registered none-ls source names
local function get_none_ls_sources()
  if not is_none_ls_loaded() then
    return {}
  end

  local sources = {}
  local null_ls = require('null-ls')
  
  -- Collect source names from builtins
  if null_ls.builtins and null_ls.builtins.diagnostics then
    for name, _ in pairs(null_ls.builtins.diagnostics) do
      sources[name] = true
    end
  end

  return sources
end

-- Check if diagnostic is from none-ls source
local function is_none_ls_diagnostic(diagnostic)
  if not diagnostic.source then
    return false
  end

  local sources = get_none_ls_sources()
  return sources[diagnostic.source] == true
end

-- Get diagnostics for buffer from none-ls sources only
function NoneLsProvider:get_diagnostics(bufnr)
  if not is_none_ls_loaded() then
    return {}
  end

  local all_diagnostics = vim.diagnostic.get(bufnr)
  local none_ls_diagnostics = {}

  for _, diagnostic in ipairs(all_diagnostics) do
    if is_none_ls_diagnostic(diagnostic) then
      table.insert(none_ls_diagnostics, diagnostic)
    end
  end

  return none_ls_diagnostics
end

-- Get code actions for buffer and range from none-ls
function NoneLsProvider:get_code_actions(bufnr, range)
  -- none-ls code actions are integrated into vim.lsp.buf.code_action()
  -- We don't need a separate implementation
  return {}
end

-- Get Treesitter node for scope
-- none-ls doesn't provide scope detection, delegates to TreesitterProvider
function NoneLsProvider:get_scope_node(bufnr, pos, scope_type)
  return nil
end

-- Check if diagnostic can be suppressed
-- none-ls diagnostics can be suppressed if they're from a registered source
function NoneLsProvider:can_suppress(diagnostic)
  return is_none_ls_diagnostic(diagnostic)
end

-- Suppress diagnostic at specified scope
-- Delegates to language-specific adapters
function NoneLsProvider:suppress(diagnostic, scope)
  -- This will delegate to language adapters in the diagnostics feature
  -- For now, return false (not implemented yet)
  return false
end

-- Check if provider is available for buffer
-- Available if none-ls is loaded
function NoneLsProvider:is_available(bufnr)
  return is_none_ls_loaded()
end

return NoneLsProvider

-- ABOUTME: Central manager for diagnostic suppression coordination
-- ABOUTME: Orchestrates providers, storage, and scope detection for suppression

local Storage = require('lsp.features.diagnostics.storage')
local ScopeDetector = require('lsp.features.diagnostics.scope_detector')

local M = {}

local SuppressionManager = {}
SuppressionManager.__index = SuppressionManager

-- Create new suppression manager
-- @param opts table: { lsp_provider, ts_provider, none_ls_provider, config_path?, adapters? }
function SuppressionManager:new(opts)
  local o = {}
  setmetatable(o, self)
  self.__index = self

  o.lsp_provider = opts.lsp_provider
  o.ts_provider = opts.ts_provider
  o.none_ls_provider = opts.none_ls_provider
  o.adapters = opts.adapters or {}
  
  o.storage = Storage:new(opts.config_path)
  o.scope_detector = ScopeDetector:new(opts.ts_provider)

  return o
end

-- Suppress a diagnostic at the specified scope
-- @param bufnr number: Buffer number
-- @param file string: File path
-- @param diagnostic table: Diagnostic object
-- @param scope string: Scope level (statement|function|class|file|project)
-- @return boolean: True if suppression was successful
function SuppressionManager:suppress_diagnostic(bufnr, file, diagnostic, scope)
  -- Check if diagnostic can be suppressed
  local can_suppress = self.lsp_provider:can_suppress(diagnostic) or
                       self.none_ls_provider:can_suppress(diagnostic)

  if not can_suppress then
    return false
  end

  -- For line-specific scopes, get the line from diagnostic
  local line = nil
  if scope ~= "file" and scope ~= "project" then
    line = diagnostic.lnum
  end

  -- Add suppression to storage
  self.storage:add_suppression({
    source = diagnostic.source,
    code = diagnostic.code,
    scope = scope,
    file = file,
    line = line
  })

  return true
end

-- Check if a diagnostic is suppressed
-- @param file string: File path
-- @param diagnostic table: Diagnostic object
-- @return boolean: True if diagnostic is suppressed
function SuppressionManager:is_diagnostic_suppressed(file, diagnostic)
  return self.storage:is_suppressed(file, diagnostic)
end

-- Filter diagnostics, removing suppressed ones
-- @param file string: File path
-- @param diagnostics table: List of diagnostics
-- @return table: Filtered list of diagnostics
function SuppressionManager:filter_diagnostics(file, diagnostics)
  local filtered = {}

  for _, diagnostic in ipairs(diagnostics) do
    if not self:is_diagnostic_suppressed(file, diagnostic) then
      table.insert(filtered, diagnostic)
    end
  end

  return filtered
end

-- Get diagnostic at cursor position
-- @param bufnr number: Buffer number
-- @param pos table: Cursor position {row, col}
-- @return table|nil: Diagnostic at cursor or nil
function SuppressionManager:get_diagnostic_at_cursor(bufnr, pos)
  local diagnostics = vim.diagnostic.get(bufnr)
  local row, col = pos[1], pos[2]

  for _, diagnostic in ipairs(diagnostics) do
    if diagnostic.lnum == row then
      -- Check if cursor is within diagnostic range
      -- If end_col is not provided, consider cursor anywhere on the line after col as matching
      local end_col = diagnostic.end_col or math.huge
      if col >= diagnostic.col and col <= end_col then
        return diagnostic
      end
    end
  end

  return nil
end

-- Get available scopes at cursor position
-- @param bufnr number: Buffer number
-- @param pos table: Cursor position {row, col}
-- @return table: List of available scopes
function SuppressionManager:get_available_scopes(bufnr, pos)
  return self.scope_detector:get_available_scopes(bufnr, pos)
end

-- Suppress diagnostic by inserting language-specific comment
-- @param bufnr number: Buffer number
-- @param file string: File path
-- @param diagnostic table: Diagnostic object
-- @param position string: Where to insert comment (inline|above|file)
-- @return boolean: True if comment was inserted
function SuppressionManager:suppress_with_comment(bufnr, file, diagnostic, position)
  -- Get filetype from buffer
  local filetype = vim.api.nvim_buf_get_option(bufnr, 'filetype')
  
  -- Find appropriate adapter
  local adapter = self.adapters[filetype]
  if not adapter then
    return false
  end
  
  -- Check if adapter supports this diagnostic source
  if not adapter:supports_source(diagnostic.source) then
    return false
  end
  
  -- Insert comment using adapter
  adapter:insert_suppression_comment(bufnr, diagnostic, position)
  
  -- Also store in suppression storage
  local scope = position == "file" and "file" or "statement"
  self:suppress_diagnostic(bufnr, file, diagnostic, scope)
  
  return true
end

M = SuppressionManager

return M

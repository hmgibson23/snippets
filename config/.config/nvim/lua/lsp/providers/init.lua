-- ABOUTME: Base provider class for LSP data abstraction
-- ABOUTME: Defines common interface for LSP, none-ls, and Treesitter providers

local M = {}

-- Provider base class
local Provider = {}
Provider.__index = Provider

-- Create new provider instance
function Provider:new(o)
  o = o or {}
  setmetatable(o, self)
  self.__index = self
  return o
end

-- Get diagnostics for buffer
-- @param bufnr number: Buffer number
-- @return table: List of diagnostics
function Provider:get_diagnostics(bufnr)
  error("Provider:get_diagnostics() must be implemented by subclass")
end

-- Get code actions for buffer and range
-- @param bufnr number: Buffer number
-- @param range table|nil: Optional range {start_pos, end_pos}
-- @return table: List of code actions
function Provider:get_code_actions(bufnr, range)
  error("Provider:get_code_actions() must be implemented by subclass")
end

-- Get Treesitter node for scope
-- @param bufnr number: Buffer number
-- @param pos table: Cursor position {row, col}
-- @param scope_type string: Scope type (statement|function|class|file)
-- @return TSNode|nil: Treesitter node or nil
function Provider:get_scope_node(bufnr, pos, scope_type)
  error("Provider:get_scope_node() must be implemented by subclass")
end

-- Check if diagnostic can be suppressed
-- @param diagnostic table: Diagnostic object
-- @return boolean: True if can be suppressed
function Provider:can_suppress(diagnostic)
  error("Provider:can_suppress() must be implemented by subclass")
end

-- Suppress diagnostic at specified scope
-- @param diagnostic table: Diagnostic object
-- @param scope string: Scope level (statement|function|class|file|project)
-- @return boolean: True if suppression was successful
function Provider:suppress(diagnostic, scope)
  error("Provider:suppress() must be implemented by subclass")
end

-- Check if provider is available for buffer
-- @param bufnr number: Buffer number
-- @return boolean: True if provider is available
function Provider:is_available(bufnr)
  error("Provider:is_available() must be implemented by subclass")
end

-- Export the Provider class
M = Provider

return M

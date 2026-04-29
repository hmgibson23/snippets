-- ABOUTME: Scope detection using Treesitter for diagnostic suppression
-- ABOUTME: Identifies code scope boundaries (statement, function, class, file)

local M = {}

local ScopeDetector = {}
ScopeDetector.__index = ScopeDetector

-- Create new scope detector
-- @param treesitter_provider TreesitterProvider: Provider for treesitter operations
function ScopeDetector:new(treesitter_provider)
  local o = {}
  setmetatable(o, self)
  self.__index = self

  o.ts_provider = treesitter_provider

  return o
end

-- Get range for a specific scope type
-- @param bufnr number: Buffer number
-- @param pos table: Cursor position {row, col}
-- @param scope_type string: Scope type (statement|function|class|file)
-- @return table|nil: Range {start_line, start_col, end_line, end_col} or nil
function ScopeDetector:get_scope_range(bufnr, pos, scope_type)
  local node = self.ts_provider:get_scope_node(bufnr, pos, scope_type)
  
  if not node then
    return nil
  end

  local start_row, start_col, end_row, end_col = node:range()
  
  return {
    start_line = start_row,
    start_col = start_col,
    end_line = end_row,
    end_col = end_col
  }
end

-- Get all available scope levels at cursor position
-- @param bufnr number: Buffer number
-- @param pos table: Cursor position {row, col}
-- @return table: List of available scope types {name, range}
function ScopeDetector:get_available_scopes(bufnr, pos)
  local scopes = {}
  local scope_types = { "statement", "function", "class" }

  for _, scope_type in ipairs(scope_types) do
    local range = self:get_scope_range(bufnr, pos, scope_type)
    if range then
      table.insert(scopes, {
        name = scope_type,
        range = range
      })
    end
  end

  -- File scope is always available
  table.insert(scopes, {
    name = "file",
    range = nil -- File scope doesn't have a specific range
  })

  return scopes
end

-- Check if line is within scope range
-- @param line number: Line number (0-indexed)
-- @param range table: Scope range {start_line, end_line}
-- @return boolean: True if line is within range
function ScopeDetector:is_line_in_scope(line, range)
  if not range then
    return false
  end

  return line >= range.start_line and line <= range.end_line
end

M = ScopeDetector

return M

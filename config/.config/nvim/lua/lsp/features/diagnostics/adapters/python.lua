-- ABOUTME: Python-specific adapter for generating diagnostic suppression comments
-- ABOUTME: Generates appropriate suppression syntax for pylint, mypy, pyright, ruff, flake8

local M = {}

local PythonAdapter = {}
PythonAdapter.__index = PythonAdapter

-- Mapping of sources to their suppression comment formats
local SUPPRESSION_FORMATS = {
  pylint = function(code)
    return string.format("# pylint: disable=%s", code or "all")
  end,
  mypy = function(code)
    if code then
      return string.format("# type: ignore[%s]", code)
    else
      return "# type: ignore"
    end
  end,
  pyright = function(code)
    if code then
      return string.format("# pyright: ignore[%s]", code)
    else
      return "# type: ignore"  -- Pyright respects type: ignore comments
    end
  end,
  ruff = function(code)
    if code then
      return string.format("# noqa: %s", code)
    else
      return "# noqa"
    end
  end,
  flake8 = function(code)
    if code then
      return string.format("# noqa: %s", code)
    else
      return "# noqa"
    end
  end,
}

-- Create new Python adapter
function PythonAdapter:new()
  local o = {}
  setmetatable(o, self)
  self.__index = self
  return o
end

-- Get the suppression comment for a diagnostic
-- @param diagnostic table: Diagnostic object with source and code
-- @return string: Suppression comment
function PythonAdapter:get_suppression_comment(diagnostic)
  local source = diagnostic.source
  local code = diagnostic.code
  
  -- Find the appropriate format function
  local format_fn = SUPPRESSION_FORMATS[source]
  
  if not format_fn then
    -- Default to noqa for unknown sources
    format_fn = SUPPRESSION_FORMATS.ruff
  end
  
  return format_fn(code)
end

-- Insert suppression comment into buffer
-- @param bufnr number: Buffer number
-- @param diagnostic table: Diagnostic object
-- @param position string: Where to insert ("inline"|"above"|"file")
function PythonAdapter:insert_suppression_comment(bufnr, diagnostic, position)
  local comment = self:get_suppression_comment(diagnostic)
  local line_num = diagnostic.lnum
  
  if position == "inline" then
    -- Append to end of line
    local line = vim.api.nvim_buf_get_lines(bufnr, line_num, line_num + 1, false)[1]
    local new_line = string.format("%s  %s", line, comment)
    vim.api.nvim_buf_set_lines(bufnr, line_num, line_num + 1, false, { new_line })
    
  elseif position == "above" then
    -- Insert on line before
    local indent = self:_get_line_indent(bufnr, line_num)
    local new_line = string.format("%s%s", indent, comment)
    vim.api.nvim_buf_set_lines(bufnr, line_num, line_num, false, { new_line })
    
  elseif position == "file" then
    -- Insert at top of file
    vim.api.nvim_buf_set_lines(bufnr, 0, 0, false, { comment })
  end
end

-- Get the indentation of a line
-- @param bufnr number: Buffer number
-- @param line_num number: Line number (0-indexed)
-- @return string: Indentation whitespace
function PythonAdapter:_get_line_indent(bufnr, line_num)
  local line = vim.api.nvim_buf_get_lines(bufnr, line_num, line_num + 1, false)[1]
  local indent = line:match("^(%s*)")
  return indent or ""
end

-- Check if this adapter supports a given diagnostic source
-- @param source string: Diagnostic source name
-- @return boolean: True if supported
function PythonAdapter:supports_source(source)
  return SUPPRESSION_FORMATS[source] ~= nil
end

-- Get the filetype this adapter supports
-- @return string: Filetype name
function PythonAdapter:get_filetype()
  return "python"
end

M = PythonAdapter

return M

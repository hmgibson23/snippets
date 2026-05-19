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

local function is_coding_comment(line)
  return line and line:match("coding[:=]%s*[-%w_.]+") ~= nil
end

local function is_docstring_start(line)
  return line and line:match('^%s*[rubfRUBF]*["\']%s*["\']%s*["\']') ~= nil
end

function PythonAdapter:_file_suppression_insert_line(bufnr)
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  local index = 0

  if lines[index + 1] and lines[index + 1]:match("^#!") then
    index = index + 1
  end

  if is_coding_comment(lines[index + 1]) then
    index = index + 1
  end

  if is_docstring_start(lines[index + 1]) then
    local quote = lines[index + 1]:match('(["\']["\']["\'])')
    index = index + 1
    if quote and not lines[index]:match(quote .. "%s*$") then
      while lines[index + 1] do
        local line = lines[index + 1]
        index = index + 1
        if line:find(quote, 1, true) then
          break
        end
      end
    end
  end

  return index
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
    -- Insert after shebang/encoding/module docstring so Python file metadata remains valid.
    local insert_at = self:_file_suppression_insert_line(bufnr)
    vim.api.nvim_buf_set_lines(bufnr, insert_at, insert_at, false, { comment })
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

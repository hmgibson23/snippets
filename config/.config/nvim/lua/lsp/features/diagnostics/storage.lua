-- ABOUTME: Persistent storage for diagnostic suppressions
-- ABOUTME: Manages reading/writing suppression rules to .nvim/diagnostics.json

local M = {}

-- Storage class for managing diagnostic suppressions
local Storage = {}
Storage.__index = Storage

-- Create new storage instance
-- @param config_path string: Path to config file (defaults to .nvim/diagnostics.json)
function Storage:new(config_path)
  local o = {}
  setmetatable(o, self)
  self.__index = self

  -- Use provided path or default to .nvim/diagnostics.json in project root
  o.config_path = config_path or self:get_default_config_path()
  o.suppressions = self:load_suppressions(o.config_path)

  return o
end

-- Get default config path (.nvim/diagnostics.json in project root)
function Storage:get_default_config_path()
  local root = vim.fn.getcwd()
  local nvim_dir = root .. "/.nvim"
  
  -- Create .nvim directory if it doesn't exist
  if vim.fn.isdirectory(nvim_dir) == 0 then
    vim.fn.mkdir(nvim_dir, "p")
  end

  return nvim_dir .. "/diagnostics.json"
end

-- Load suppressions from disk
function Storage:load_suppressions(path)
  if vim.fn.filereadable(path) == 0 then
    return { files = {} }
  end

  local content = vim.fn.readfile(path)
  if #content == 0 then
    return { files = {} }
  end

  local ok, data = pcall(vim.json.decode, table.concat(content, "\n"))
  if not ok then
    vim.notify("Failed to parse diagnostics config: " .. path, vim.log.levels.WARN)
    return { files = {} }
  end

  return data
end

-- Save suppressions to disk
function Storage:save_suppressions()
  local content = vim.json.encode(self.suppressions)
  vim.fn.writefile({ content }, self.config_path)
end

-- Add suppression rule
-- @param suppression table: { source, code, scope, file, line? }
function Storage:add_suppression(suppression)
  local file = suppression.file
  
  if not self.suppressions.files[file] then
    self.suppressions.files[file] = {}
  end

  table.insert(self.suppressions.files[file], {
    source = suppression.source,
    code = suppression.code,
    scope = suppression.scope,
    line = suppression.line,
  })

  self:save_suppressions()
end

-- Remove suppression rule
-- @param suppression table: { source, code, file, line? }
function Storage:remove_suppression(suppression)
  local file = suppression.file
  local removed = 0

  local function should_remove(s, allow_project)
    local match = s.source == suppression.source and s.code == suppression.code
    if not match then
      return false
    end
    if allow_project and s.scope == "project" then
      return true
    end
    if suppression.line and s.line then
      return s.line == suppression.line
    end
    return not s.line
  end

  local function remove_from_file(target_file, allow_project)
    local file_suppressions = self.suppressions.files[target_file]
    if not file_suppressions then
      return
    end

    local filtered = {}
    for _, s in ipairs(file_suppressions) do
      if should_remove(s, allow_project) then
        removed = removed + 1
      else
        table.insert(filtered, s)
      end
    end
    self.suppressions.files[target_file] = filtered
  end

  remove_from_file(file, true)

  -- Project-level suppressions apply everywhere, so allow removing them from
  -- whichever file currently shows the suppressed diagnostic.
  if removed == 0 then
    for target_file, _ in pairs(self.suppressions.files or {}) do
      if target_file ~= file then
        remove_from_file(target_file, true)
      end
    end
  end

  self:save_suppressions()
end

-- Get all suppressions for a file
-- @param file string: File path
-- @return table: List of suppressions
function Storage:get_suppressions(file)
  return self.suppressions.files[file] or {}
end

local function suppression_matches(suppression, diagnostic)
  return suppression.source == diagnostic.source and suppression.code == diagnostic.code
end

local function line_matches(suppression, diagnostic)
  return suppression.line and suppression.line == diagnostic.lnum
end

-- Check if diagnostic is suppressed
-- @param file string: File path
-- @param diagnostic table: Diagnostic object
-- @return boolean: True if diagnostic is suppressed
function Storage:is_suppressed(file, diagnostic)
  local suppressions = self:get_suppressions(file)

  for _, suppression in ipairs(suppressions) do
    if suppression_matches(suppression, diagnostic) then
      -- File-level suppressions apply to all lines in the current file.
      if suppression.scope == "file" then
        return true
      end

      -- Line-specific suppressions.
      if line_matches(suppression, diagnostic) then
        return true
      end
    end
  end

  -- Project-level suppressions apply regardless of the file they were created from.
  for _, file_suppressions in pairs(self.suppressions.files or {}) do
    for _, suppression in ipairs(file_suppressions) do
      if suppression.scope == "project" and suppression_matches(suppression, diagnostic) then
        return true
      end
    end
  end

  return false
end

M = Storage

return M

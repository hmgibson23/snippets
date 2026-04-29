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
  
  if not self.suppressions.files[file] then
    return
  end

  local filtered = {}
  for _, s in ipairs(self.suppressions.files[file]) do
    local match = s.source == suppression.source and s.code == suppression.code
    if suppression.line then
      match = match and s.line == suppression.line
    end
    
    if not match then
      table.insert(filtered, s)
    end
  end

  self.suppressions.files[file] = filtered
  self:save_suppressions()
end

-- Get all suppressions for a file
-- @param file string: File path
-- @return table: List of suppressions
function Storage:get_suppressions(file)
  return self.suppressions.files[file] or {}
end

-- Check if diagnostic is suppressed
-- @param file string: File path
-- @param diagnostic table: Diagnostic object
-- @return boolean: True if diagnostic is suppressed
function Storage:is_suppressed(file, diagnostic)
  local suppressions = self:get_suppressions(file)

  for _, suppression in ipairs(suppressions) do
    local source_match = suppression.source == diagnostic.source
    local code_match = suppression.code == diagnostic.code

    if source_match and code_match then
      -- File-level suppressions apply to all lines
      if suppression.scope == "file" then
        return true
      end

      -- Line-specific suppressions
      if suppression.line and suppression.line == diagnostic.lnum then
        return true
      end
    end
  end

  return false
end

M = Storage

return M

-- ABOUTME: Detects Python project context for DAP debugging
-- ABOUTME: Identifies uv projects, FastAPI servers, and determines file types

local M = {}

-- Cache detection results per buffer
local _cache = {}

--- Find the project root by looking for pyproject.toml
--- @param filepath string The current file path
--- @return string|nil The project root directory or nil if not found
function M.find_project_root(filepath)
  local current_dir = vim.fn.fnamemodify(filepath, ':p:h')
  local root_dir = current_dir
  
  -- Walk up the directory tree
  while root_dir ~= '/' do
    local pyproject_path = root_dir .. '/pyproject.toml'
    local stat = vim.loop.fs_stat(pyproject_path)
    
    if stat and stat.type == 'file' then
      return root_dir
    end
    
    -- Move up one directory
    local parent = vim.fn.fnamemodify(root_dir, ':h')
    if parent == root_dir then
      break
    end
    root_dir = parent
  end
  
  return nil
end

--- Detect the type of Python file (script or fastapi)
--- @param content string The file content as a single string
--- @return string 'fastapi' or 'script'
function M.detect_file_type(content)
  -- FastAPI patterns to look for
  local fastapi_patterns = {
    'from%s+fastapi%s+import',
    'import%s+fastapi',
    'FastAPI%(%)',
    'uvicorn%.run',
    '@app%.get',
    '@app%.post',
    '@app%.put',
    '@app%.delete',
    '@app%.patch',
  }
  
  for _, pattern in ipairs(fastapi_patterns) do
    if content:match(pattern) then
      return 'fastapi'
    end
  end
  
  return 'script'
end

--- Detect full context for the current buffer
--- @param bufnr number|nil Buffer number (defaults to current buffer)
--- @return table Context information with fields: project_root, file_type, is_uv_project, filepath
function M.detect_context(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  
  -- Check cache
  if _cache[bufnr] then
    return _cache[bufnr]
  end
  
  local filepath = vim.api.nvim_buf_get_name(bufnr)
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  local content = table.concat(lines, '\n')
  
  local project_root = M.find_project_root(filepath)
  local file_type = M.detect_file_type(content)
  
  local context = {
    filepath = filepath,
    project_root = project_root,
    file_type = file_type,
    is_uv_project = project_root ~= nil,
  }
  
  -- Cache the result
  _cache[bufnr] = context
  
  return context
end

--- Clear cache for a buffer
--- @param bufnr number|nil Buffer number (defaults to current buffer)
function M.clear_cache(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  _cache[bufnr] = nil
end

--- Clear all caches
function M.clear_all_caches()
  _cache = {}
end

-- Setup autocmd to clear cache on buffer save
vim.api.nvim_create_autocmd('BufWritePost', {
  pattern = '*.py',
  callback = function(args)
    M.clear_cache(args.buf)
  end,
  group = vim.api.nvim_create_augroup('DapPythonDetector', { clear = true }),
})

return M

-- ABOUTME: Builds DAP configurations for Python debugging
-- ABOUTME: Handles script and FastAPI server launch configurations

local M = {}

--- Find the FastAPI entry point in a project
--- @param project_root string The project root directory
--- @return string|nil Entry point in format "module:app" or nil if not found
function M.find_fastapi_entry(project_root)
  -- Common FastAPI entry point file names
  local candidates = { 'main.py', 'app.py', 'api.py', 'server.py' }
  
  for _, filename in ipairs(candidates) do
    local filepath = project_root .. '/' .. filename
    local stat = vim.uv.fs_stat(filepath)
    
    if stat and stat.type == 'file' then
      -- Read the file and look for FastAPI app variable
      local lines = vim.fn.readfile(filepath)
      local content = table.concat(lines, '\n')
      
      -- Look for pattern like: app = FastAPI() or application = FastAPI()
      local app_var = content:match('([%w_]+)%s*=%s*FastAPI%s*%(')
      
      if app_var then
        -- Extract module name (filename without .py)
        local module = filename:match('(.+)%.py$')
        return module .. ':' .. app_var
      end
    end
  end
  
  return nil
end

--- Build a DAP configuration for the given context and interpreter
--- @param context table Context from detector.detect_context()
--- @param interpreter string Interpreter path or 'uv' marker
--- @return table DAP configuration
function M.build_config(context, interpreter)
  local config = {
    type = 'python',
    request = 'launch',
    justMyCode = false,
  }
  
  -- Handle script vs FastAPI
  if context.file_type == 'script' then
    config.name = 'Python: Current File'
    config.program = context.filepath
    
    if context.project_root then
      config.cwd = context.project_root
    end
    
  elseif context.file_type == 'fastapi' then
    config.name = 'FastAPI: Run Server'
    config.module = 'uvicorn'
    
    if context.project_root then
      config.cwd = context.project_root
      local entry = M.find_fastapi_entry(context.project_root)
      
      if entry then
        config.args = { entry, '--reload' }
      else
        -- Fallback: try to guess from current file
        local module = vim.fn.fnamemodify(context.filepath, ':t:r')
        config.args = { module .. ':app', '--reload' }
      end
    end
  end
  
  -- Handle interpreter
  if interpreter == 'uv' then
    config.python = { 'uv', 'run', 'python' }
  else
    config.pythonPath = interpreter
  end
  
  return config
end

return M

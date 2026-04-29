-- ABOUTME: Coordinates Python DAP auto-configuration
-- ABOUTME: Main entry point for smart Python debugging

local detector = require('dap.python.detector')
local interpreter = require('dap.python.interpreter')
local launcher = require('dap.python.launcher')

local M = {}

--- Setup auto-configuration for the current Python buffer
--- @return table|nil DAP configuration or nil on error
function M.setup_auto_config()
  local bufnr = vim.api.nvim_get_current_buf()
  
  -- Only work with Python files
  local filetype = vim.bo[bufnr].filetype
  if filetype ~= 'python' then
    vim.notify('DAP Python: Not a Python file', vim.log.levels.WARN)
    return nil
  end
  
  -- Detect context
  local context = detector.detect_context(bufnr)
  
  -- Resolve interpreter
  local python_interpreter = interpreter.resolve(context)
  if not python_interpreter then
    vim.notify('DAP Python: No Python interpreter found', vim.log.levels.ERROR)
    return nil
  end
  
  -- Build configuration
  local config = launcher.build_config(context, python_interpreter)
  
  -- Debug logging (can be removed later)
  if vim.env.DEBUG_DAP then
    vim.notify(
      string.format(
        'DAP Python Config:\n  Type: %s\n  Project: %s\n  Interpreter: %s',
        context.file_type,
        context.project_root or 'none',
        python_interpreter
      ),
      vim.log.levels.INFO
    )
  end
  
  return config
end

--- Start debugging with auto-configuration
function M.start()
  local config = M.setup_auto_config()
  if not config then
    return
  end
  
  local dap = require('dap')
  
  -- Set the configuration for python adapter
  dap.configurations.python = dap.configurations.python or {}
  table.insert(dap.configurations.python, 1, config)
  
  -- Start debugging
  dap.continue()
end

--- Continue debugging with auto-configuration (for first start)
function M.continue()
  local dap = require('dap')
  
  -- If session already active, just continue
  if dap.session() then
    dap.continue()
    return
  end
  
  -- Otherwise, start a new session
  M.start()
end

return M

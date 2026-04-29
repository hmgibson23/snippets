-- ABOUTME: Resolves Python interpreter path for DAP debugging
-- ABOUTME: Handles uv projects, venv detection, and fallback resolution

local M = {}

--- Resolve the Python interpreter for the given context
--- @param context table Context from detector.detect_context()
--- @return string|nil Python interpreter path or 'uv' marker for uv projects
function M.resolve(context)
  -- For uv projects, return special marker
  if context.is_uv_project then
    return 'uv'
  end
  
  -- Try to get from venv-selector plugin
  local ok, venv_selector = pcall(require, 'venv-selector')
  if ok and venv_selector.venv then
    local venv_path = venv_selector.venv()
    if venv_path then
      return venv_path
    end
  end
  
  -- Fall back to system Python
  if vim.fn.executable('python3') == 1 then
    return 'python3'
  end
  
  if vim.fn.executable('python') == 1 then
    return 'python'
  end
  
  -- No Python found
  return nil
end

return M

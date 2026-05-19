-- ABOUTME: User commands for diagnostic suppression system
-- ABOUTME: Provides interactive commands to suppress diagnostics at cursor

local SuppressionManager = require('lsp.features.diagnostics.manager')
local LspProvider = require('lsp.providers.lsp')
local TreesitterProvider = require('lsp.providers.treesitter')
local NoneLsProvider = require('lsp.providers.none_ls')
local PythonAdapter = require('lsp.features.diagnostics.adapters.python')
local filter = require('lsp.features.diagnostics.filter')

local M = {}

-- Initialize the suppression system
function M.setup(opts)
  opts = opts or {}
  
  -- Create providers
  local lsp_provider = LspProvider:new()
  local ts_provider = TreesitterProvider:new()
  local none_ls_provider = NoneLsProvider:new()
  
  -- Create adapters
  local adapters = {
    python = PythonAdapter:new()
  }
  
  -- Create manager
  M.manager = SuppressionManager:new({
    lsp_provider = lsp_provider,
    ts_provider = ts_provider,
    none_ls_provider = none_ls_provider,
    config_path = opts.config_path,
    adapters = adapters
  })
  
  -- Create user commands
  M._create_commands()
end

-- Create vim commands
function M._create_commands()
  -- Suppress diagnostic at cursor with inline comment
  vim.api.nvim_create_user_command('SuppressDiagnostic', function()
    M.suppress_at_cursor('inline')
  end, { desc = 'Suppress diagnostic at cursor with inline comment' })
  
  -- Suppress diagnostic with comment above line
  vim.api.nvim_create_user_command('SuppressDiagnosticAbove', function()
    M.suppress_at_cursor('above')
  end, { desc = 'Suppress diagnostic with comment above line' })
  
  -- Suppress diagnostic at file level
  vim.api.nvim_create_user_command('SuppressDiagnosticFile', function()
    M.suppress_at_cursor('file')
  end, { desc = 'Suppress diagnostic at file level' })
  
  -- Remove suppression
  vim.api.nvim_create_user_command('UnsuppressDiagnostic', function()
    M.unsuppress_at_cursor()
  end, { desc = 'Remove suppression at cursor' })
end

-- Suppress diagnostic at cursor position
-- @param position string: Where to insert comment (inline|above|file)
function M.suppress_at_cursor(position)
  if not M.manager then
    vim.notify("Suppression system not initialized. Call setup() first.", vim.log.levels.ERROR)
    return
  end
  
  local bufnr = vim.api.nvim_get_current_buf()
  local file = vim.api.nvim_buf_get_name(bufnr)
  local cursor = vim.api.nvim_win_get_cursor(0)
  
  -- Convert from 1-indexed to 0-indexed
  local pos = { cursor[1] - 1, cursor[2] }
  
  -- Get diagnostic at cursor
  local diagnostic = M.manager:get_diagnostic_at_cursor(bufnr, pos)
  
  if not diagnostic then
    vim.notify("No diagnostic at cursor position", vim.log.levels.WARN)
    return
  end
  
  -- Try to suppress with comment first
  local success = M.manager:suppress_with_comment(bufnr, file, diagnostic, position)
  
  if success then
    filter.refresh(bufnr)
    vim.notify(string.format("Suppressed %s: %s", diagnostic.source, diagnostic.code), vim.log.levels.INFO)
  else
    -- Fall back to storage-only suppression
    local scope = position == "file" and "file" or "statement"
    success = M.manager:suppress_diagnostic(bufnr, file, diagnostic, scope)
    
    if success then
      filter.refresh(bufnr)
      vim.notify(string.format("Suppressed %s: %s (no comment adapter available)", 
                               diagnostic.source, diagnostic.code), vim.log.levels.INFO)
    else
      vim.notify("Failed to suppress diagnostic", vim.log.levels.ERROR)
    end
  end
end

-- Remove suppression at cursor position
function M.unsuppress_at_cursor()
  if not M.manager then
    vim.notify("Suppression system not initialized. Call setup() first.", vim.log.levels.ERROR)
    return
  end
  
  local bufnr = vim.api.nvim_get_current_buf()
  local file = vim.api.nvim_buf_get_name(bufnr)
  local cursor = vim.api.nvim_win_get_cursor(0)
  
  -- Convert from 1-indexed to 0-indexed
  local pos = { cursor[1] - 1, cursor[2] }
  
  -- Get diagnostic at cursor
  local diagnostic = M.manager:get_diagnostic_at_cursor(bufnr, pos)
  
  if not diagnostic then
    vim.notify("No diagnostic at cursor position", vim.log.levels.WARN)
    return
  end
  
  -- Remove from storage
  M.manager.storage:remove_suppression({
    file = file,
    source = diagnostic.source,
    code = diagnostic.code,
    line = diagnostic.lnum,
  })
  filter.refresh(bufnr)
  
  vim.notify(string.format("Removed suppression for %s: %s", diagnostic.source, diagnostic.code), 
             vim.log.levels.INFO)
end

return M

-- ABOUTME: Main entry point for diagnostic suppression system
-- ABOUTME: Initializes providers, adapters, and commands for suppressing diagnostics

local commands = require('lsp.features.diagnostics.commands')
local keymaps = require('lsp.features.diagnostics.keymaps')
local ui = require('lsp.features.diagnostics.ui')

local M = {}

-- Setup the diagnostic suppression system
-- @param opts table: Configuration options
--   - config_path: Path to store suppressions (default: .nvim/diagnostics.json)
--   - keybindings: Keybinding configuration (passed to keymaps.setup)
function M.setup(opts)
  opts = opts or {}
  
  -- Set default config path if not provided
  if not opts.config_path then
    local cwd = vim.fn.getcwd()
    opts.config_path = cwd .. "/.nvim/diagnostics.json"
  end
  
  -- Initialize the command system
  commands.setup(opts)
  
  -- Pass manager reference to UI module
  ui.set_manager(commands.manager)
  
  -- Set up keybindings
  keymaps.setup(opts.keybindings)
  
  -- Set up autocommand to filter diagnostics
  M._setup_diagnostic_filter(commands.manager)
end

-- Setup diagnostic filtering
-- @param manager SuppressionManager: The manager instance
function M._setup_diagnostic_filter(manager)
  -- Create autocommand group
  local group = vim.api.nvim_create_augroup('DiagnosticSuppression', { clear = true })
  
  -- Filter diagnostics on buffer attach
  vim.api.nvim_create_autocmd({ 'DiagnosticChanged', 'BufEnter' }, {
    group = group,
    callback = function(args)
      local bufnr = args.buf
      local file = vim.api.nvim_buf_get_name(bufnr)
      
      if file == '' then
        return
      end
      
      -- Get current diagnostics
      local diagnostics = vim.diagnostic.get(bufnr)
      
      -- Filter out suppressed diagnostics
      local filtered = manager:filter_diagnostics(file, diagnostics)
      
      -- Only update if there's a difference
      if #filtered ~= #diagnostics then
        -- Set the filtered diagnostics
        vim.diagnostic.set(vim.api.nvim_create_namespace('diagnostic_suppression'), 
                          bufnr, filtered, {})
      end
    end,
  })
end

return M

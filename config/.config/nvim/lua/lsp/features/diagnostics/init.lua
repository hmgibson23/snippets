-- ABOUTME: Main entry point for diagnostic suppression system
-- ABOUTME: Initializes providers, adapters, and commands for suppressing diagnostics

local commands = require('lsp.features.diagnostics.commands')
local keymaps = require('lsp.features.diagnostics.keymaps')
local ui = require('lsp.features.diagnostics.ui')
local filter = require('lsp.features.diagnostics.filter')

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

  -- Filter rendered diagnostics without mutating producer namespaces.
  filter.install(commands.manager)
end

return M

-- ABOUTME: Keybindings for diagnostic suppression
-- ABOUTME: Provides convenient keymaps for suppressing diagnostics at cursor

local M = {}

-- Setup keybindings
-- @param opts table: Configuration options
--   - prefix: Key prefix (default: <leader>d)
--   - mappings: Custom mappings override
function M.setup(opts)
  opts = opts or {}
  local prefix = opts.prefix or '<leader>d'
  
  -- Default mappings
  local mappings = opts.mappings or {
    suppress_inline = 's',
    suppress_above = 'S',
    suppress_file = 'f',
    unsuppress = 'u',
    suppress_interactive = 'i',
    list_suppressions = 'l',
  }
  
  -- Suppress inline
  vim.keymap.set('n', prefix .. mappings.suppress_inline, function()
    vim.cmd('SuppressDiagnostic')
  end, { desc = 'Suppress diagnostic inline' })
  
  -- Suppress above
  vim.keymap.set('n', prefix .. mappings.suppress_above, function()
    vim.cmd('SuppressDiagnosticAbove')
  end, { desc = 'Suppress diagnostic above' })
  
  -- Suppress file-level
  vim.keymap.set('n', prefix .. mappings.suppress_file, function()
    vim.cmd('SuppressDiagnosticFile')
  end, { desc = 'Suppress diagnostic (file level)' })
  
  -- Unsuppress
  vim.keymap.set('n', prefix .. mappings.unsuppress, function()
    vim.cmd('UnsuppressDiagnostic')
  end, { desc = 'Remove diagnostic suppression' })
  
  -- Interactive scope selection
  vim.keymap.set('n', prefix .. mappings.suppress_interactive, function()
    require('lsp.features.diagnostics.ui').show_scope_picker()
  end, { desc = 'Suppress diagnostic (interactive)' })
  
  -- List suppressions
  vim.keymap.set('n', prefix .. mappings.list_suppressions, function()
    require('lsp.features.diagnostics.ui').show_suppressions()
  end, { desc = 'List all suppressions' })
end

return M

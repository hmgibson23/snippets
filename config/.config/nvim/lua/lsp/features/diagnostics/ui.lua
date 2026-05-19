-- ABOUTME: UI components for diagnostic suppression
-- ABOUTME: Provides interactive pickers and visual feedback for suppressions

local filter = require('lsp.features.diagnostics.filter')

local M = {}

-- Reference to the manager (will be set during setup)
M.manager = nil

-- Set the manager instance
function M.set_manager(manager)
  M.manager = manager
end

-- Show interactive scope picker for suppression
function M.show_scope_picker()
  if not M.manager then
    vim.notify("Suppression system not initialized", vim.log.levels.ERROR)
    return
  end
  
  local bufnr = vim.api.nvim_get_current_buf()
  local file = vim.api.nvim_buf_get_name(bufnr)
  local cursor = vim.api.nvim_win_get_cursor(0)
  local pos = { cursor[1] - 1, cursor[2] }
  
  -- Get diagnostic at cursor
  local diagnostic = M.manager:get_diagnostic_at_cursor(bufnr, pos)
  
  if not diagnostic then
    vim.notify("No diagnostic at cursor position", vim.log.levels.WARN)
    return
  end
  
  -- Get available scopes
  local scopes = M.manager:get_available_scopes(bufnr, pos)
  
  -- Add file and project scopes (always available)
  table.insert(scopes, "file")
  table.insert(scopes, "project")
  
    M._show_vim_select_picker(scopes, diagnostic, bufnr, file)
end

-- Show vim.ui.select picker for scope selection. Snacks.input/select enhances this globally.
function M._show_vim_select_picker(scopes, diagnostic, bufnr, file)
  vim.ui.select(scopes, {
    prompt = string.format('Suppress %s: %s - Select scope:', diagnostic.source, diagnostic.code),
    format_item = function(scope)
      return M._format_scope_display(scope)
    end
  }, function(choice)
    if choice then
      M._suppress_with_scope(bufnr, file, diagnostic, choice)
    end
  end)
end

-- Format scope for display
function M._format_scope_display(scope)
  local descriptions = {
    statement = "Statement - Current line only",
    ["function"] = "Function - Entire function",
    class = "Class - Entire class",
    file = "File - Entire file",
    project = "Project - All files"
  }
  return descriptions[scope] or scope
end

-- Suppress diagnostic with chosen scope
function M._suppress_with_scope(bufnr, file, diagnostic, scope)
  local position = "inline"
  
  if scope == "file" then
    position = "file"
  elseif scope == "statement" then
    position = "inline"
  else
    position = "above"
  end
  
  -- Try to suppress with comment
  local success = M.manager:suppress_with_comment(bufnr, file, diagnostic, position)
  
  if success then
    filter.refresh(bufnr)
    vim.notify(string.format("Suppressed %s: %s at %s scope", 
                            diagnostic.source, diagnostic.code, scope), 
               vim.log.levels.INFO)
  else
    -- Fall back to storage-only
    success = M.manager:suppress_diagnostic(bufnr, file, diagnostic, scope)
    if success then
      filter.refresh(bufnr)
      vim.notify(string.format("Suppressed %s: %s at %s scope (storage only)", 
                              diagnostic.source, diagnostic.code, scope), 
                 vim.log.levels.INFO)
    else
      vim.notify("Failed to suppress diagnostic", vim.log.levels.ERROR)
    end
  end
end

-- Show all suppressions in project
function M.show_suppressions()
  if not M.manager then
    vim.notify("Suppression system not initialized", vim.log.levels.ERROR)
    return
  end
  
  -- Get all suppressions from storage. Storage keeps rules under `files`.
  local all_suppressions = (M.manager.storage.suppressions or {}).files or {}
  
  if vim.tbl_isempty(all_suppressions) then
    vim.notify("No suppressions found", vim.log.levels.INFO)
    return
  end
  
  -- Flatten suppressions by file
  local items = {}
  for file, suppressions in pairs(all_suppressions) do
    for _, suppression in ipairs(suppressions) do
      table.insert(items, {
        file = file,
        source = suppression.source,
        code = suppression.code,
        scope = suppression.scope,
        line = suppression.line,
      })
    end
  end
  table.sort(items, function(a, b)
    if a.file == b.file then
      return (a.line or -1) < (b.line or -1)
    end
    return a.file < b.file
  end)
  
  M._show_suppressions_select(items)
end

function M._show_suppressions_select(items)
  vim.ui.select(items, {
    prompt = 'Diagnostic suppressions:',
    format_item = function(item)
      return string.format("%s:%s - %s: %s [%s]",
        vim.fn.fnamemodify(item.file, ':~:.'),
        item.line or 'file',
        item.source,
        item.code,
        item.scope)
    end,
  }, function(choice)
    if not choice then
      return
    end
    vim.cmd.edit(vim.fn.fnameescape(choice.file))
    vim.api.nvim_win_set_cursor(0, { choice.line and (choice.line + 1) or 1, 0 })
  end)
end

-- Show suppressions in quickfix list
function M._show_suppressions_quickfix(items)
  local qf_items = {}
  
  for _, item in ipairs(items) do
    table.insert(qf_items, {
      filename = item.file,
      lnum = item.line and (item.line + 1) or 1,
      text = string.format("%s: %s [%s scope]", item.source, item.code, item.scope),
      type = 'I'
    })
  end
  
  vim.fn.setqflist(qf_items)
  vim.cmd('copen')
  vim.notify(string.format("Found %d suppressions", #qf_items), vim.log.levels.INFO)
end

return M

local select = require("workflows.select")

local M = {}

local function has_command(name)
  return vim.fn.exists(":" .. name) == 2
end

function M.toggle()
  vim.diagnostic.enable(not vim.diagnostic.is_enabled())
end

function M.counts(bufnr)
  bufnr = bufnr or 0
  local counts = { errors = 0, warnings = 0, info = 0, hints = 0 }
  for _, diagnostic in ipairs(vim.diagnostic.get(bufnr)) do
    if diagnostic.severity == vim.diagnostic.severity.ERROR then
      counts.errors = counts.errors + 1
    elseif diagnostic.severity == vim.diagnostic.severity.WARN then
      counts.warnings = counts.warnings + 1
    elseif diagnostic.severity == vim.diagnostic.severity.INFO then
      counts.info = counts.info + 1
    elseif diagnostic.severity == vim.diagnostic.severity.HINT then
      counts.hints = counts.hints + 1
    end
  end
  return counts
end

function M.summary()
  local c = M.counts(0)
  vim.notify(string.format("Diagnostics: %d errors, %d warnings, %d info, %d hints", c.errors, c.warnings, c.info, c.hints))
end

function M.actions()
  local actions = {
    { label = "Line diagnostics", action = vim.diagnostic.open_float },
    {
      label = "Buffer diagnostics",
      action = function()
        if package.loaded["snacks"] then
          Snacks.picker.diagnostics_buffer()
        else
          vim.diagnostic.setloclist()
        end
      end,
    },
    {
      label = "Workspace diagnostics",
      action = function()
        if package.loaded["snacks"] then
          Snacks.picker.diagnostics()
        else
          vim.diagnostic.setqflist()
        end
      end,
    },
    { label = "Next diagnostic", action = function() vim.diagnostic.jump({ count = 1, float = true }) end },
    { label = "Previous diagnostic", action = function() vim.diagnostic.jump({ count = -1, float = true }) end },
    { label = "Toggle diagnostics", action = M.toggle },
    { label = "Diagnostic summary", action = M.summary },
  }

  if has_command("SuppressDiagnostic") then
    table.insert(actions, { label = "Suppress inline", action = function() vim.cmd("SuppressDiagnostic") end })
    table.insert(actions, { label = "Suppress above", action = function() vim.cmd("SuppressDiagnosticAbove") end })
    table.insert(actions, { label = "Suppress file", action = function() vim.cmd("SuppressDiagnosticFile") end })
    table.insert(actions, { label = "Unsuppress", action = function() vim.cmd("UnsuppressDiagnostic") end })
  end

  return actions
end

function M.palette()
  select.run("Diagnostics", M.actions())
end

function M.setup()
  local ok, suppression = pcall(require, "lsp.features.diagnostics")
  if ok then
    suppression.setup()
  end

  vim.api.nvim_create_user_command("DiagnosticPalette", M.palette, { desc = "Diagnostic action palette" })
  vim.api.nvim_create_user_command("DiagnosticSummary", M.summary, { desc = "Diagnostic counts for current buffer" })

  vim.keymap.set("n", "<leader>ld", M.palette, { desc = "Diagnostic palette" })
  vim.keymap.set("n", "<leader>lX", M.toggle, { desc = "Toggle diagnostics" })
  vim.keymap.set("n", "<leader>lC", M.summary, { desc = "Diagnostic summary" })
end

return M

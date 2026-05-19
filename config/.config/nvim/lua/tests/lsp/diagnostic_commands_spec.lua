local commands = require('lsp.features.diagnostics.commands')

describe("Diagnostic suppression commands", function()
  local original_get_current_buf
  local original_buf_get_name
  local original_win_get_cursor
  local original_notify
  local original_refresh

  before_each(function()
    original_get_current_buf = vim.api.nvim_get_current_buf
    original_buf_get_name = vim.api.nvim_buf_get_name
    original_win_get_cursor = vim.api.nvim_win_get_cursor
    original_notify = vim.notify

    local filter = require('lsp.features.diagnostics.filter')
    original_refresh = filter.refresh
    filter.refresh = function() end

    vim.api.nvim_get_current_buf = function()
      return 7
    end
    vim.api.nvim_buf_get_name = function()
      return "/tmp/test.py"
    end
    vim.api.nvim_win_get_cursor = function()
      return { 11, 0 }
    end
    vim.notify = function() end
  end)

  after_each(function()
    vim.api.nvim_get_current_buf = original_get_current_buf
    vim.api.nvim_buf_get_name = original_buf_get_name
    vim.api.nvim_win_get_cursor = original_win_get_cursor
    vim.notify = original_notify
    require('lsp.features.diagnostics.filter').refresh = original_refresh
    commands.manager = nil
  end)

  it("removes the matching suppression at the cursor", function()
    local removed
    commands.manager = {
      get_diagnostic_at_cursor = function(_, bufnr, pos)
        assert.are.equal(7, bufnr)
        assert.are.same({ 10, 0 }, pos)
        return { source = "ruff", code = "E501", lnum = 10 }
      end,
      storage = {
        remove_suppression = function(_, suppression)
          removed = suppression
        end,
      },
    }

    commands.unsuppress_at_cursor()

    assert.are.same({
      file = "/tmp/test.py",
      source = "ruff",
      code = "E501",
      line = 10,
    }, removed)
  end)
end)

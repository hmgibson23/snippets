local ui = require('lsp.features.diagnostics.ui')

describe("Diagnostic suppression UI", function()
  local original_select
  local original_cmd
  local original_set_cursor
  local selected_items

  before_each(function()
    original_select = vim.ui.select
    original_cmd = vim.cmd
    original_set_cursor = vim.api.nvim_win_set_cursor
    selected_items = nil
  end)

  after_each(function()
    vim.ui.select = original_select
    vim.cmd = original_cmd
    vim.api.nvim_win_set_cursor = original_set_cursor
    ui.set_manager(nil)
  end)

  it("lists suppressions stored under the files table", function()
    ui.set_manager({
      storage = {
        suppressions = {
          files = {
            ["/tmp/a.py"] = {
              { source = "ruff", code = "E501", scope = "file" },
            },
            ["/tmp/b.py"] = {
              { source = "pylint", code = "W0611", scope = "statement", line = 4 },
            },
          },
        },
      },
    })

    vim.ui.select = function(items, opts, on_choice)
      selected_items = items
      assert.are.equal("Diagnostic suppressions:", opts.prompt)
    end

    ui.show_suppressions()

    assert.are.equal(2, #selected_items)
    assert.are.equal("/tmp/a.py", selected_items[1].file)
    assert.are.equal("/tmp/b.py", selected_items[2].file)
  end)
end)

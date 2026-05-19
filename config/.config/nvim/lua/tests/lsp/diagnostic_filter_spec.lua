local DiagnosticFilter = require('lsp.features.diagnostics.filter')

describe('Diagnostic suppression display filter', function()
  it('passes only unsuppressed diagnostics to wrapped diagnostic handlers', function()
    local seen = nil
    local manager = {
      filter_diagnostics = function(_, file, diagnostics)
        assert.is_true(file:match('example%.py$') ~= nil)
        return vim.tbl_filter(function(diagnostic)
          return diagnostic.code ~= 'W0611'
        end, diagnostics)
      end,
    }

    local handler = DiagnosticFilter.wrap_handler(manager, {
      show = function(namespace, bufnr, diagnostics, opts)
        seen = {
          namespace = namespace,
          bufnr = bufnr,
          diagnostics = diagnostics,
          opts = opts,
        }
      end,
    })

    local bufnr = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(bufnr, '/tmp/example.py')

    handler.show(42, bufnr, {
      { source = 'pylint', code = 'W0611', lnum = 0, col = 0, message = 'unused' },
      { source = 'ruff', code = 'E501', lnum = 1, col = 0, message = 'long line' },
    }, { virtual_text = true })

    assert.are.equal(42, seen.namespace)
    assert.are.equal(bufnr, seen.bufnr)
    assert.are.equal(1, #seen.diagnostics)
    assert.are.equal('E501', seen.diagnostics[1].code)
    assert.is_true(seen.opts.virtual_text)
  end)

  it('delegates hide to the original handler', function()
    local hidden = nil
    local handler = DiagnosticFilter.wrap_handler({ filter_diagnostics = function() return {} end }, {
      hide = function(namespace, bufnr)
        hidden = { namespace = namespace, bufnr = bufnr }
      end,
    })

    handler.hide(9, 3)

    assert.are.same({ namespace = 9, bufnr = 3 }, hidden)
  end)
end)

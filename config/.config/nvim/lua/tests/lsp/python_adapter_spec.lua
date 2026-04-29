-- ABOUTME: Tests for Python-specific diagnostic suppression adapter
-- ABOUTME: Verifies generation of suppression comments for various Python linters/type checkers

local PythonAdapter = require('lsp.features.diagnostics.adapters.python')

describe("Python Adapter", function()
  local adapter

  before_each(function()
    adapter = PythonAdapter:new()
  end)

  it("can be instantiated", function()
    assert.is_not_nil(adapter)
  end)

  describe("get_suppression_comment", function()
    it("generates pylint suppression comment", function()
      local diagnostic = {
        source = "pylint",
        code = "W0611",
        message = "unused-import"
      }
      
      local comment = adapter:get_suppression_comment(diagnostic)
      assert.is_not_nil(comment)
      assert.is_true(comment:match("pylint") ~= nil)
      assert.is_true(comment:match("W0611") ~= nil)
    end)

    it("generates mypy suppression comment", function()
      local diagnostic = {
        source = "mypy",
        code = "assignment",
        message = "Incompatible types in assignment"
      }
      
      local comment = adapter:get_suppression_comment(diagnostic)
      assert.is_not_nil(comment)
      assert.is_true(comment:match("type:.*ignore") ~= nil)
    end)

    it("generates pyright suppression comment", function()
      local diagnostic = {
        source = "pyright",
        code = "reportGeneralTypeIssues",
        message = "Type issue"
      }
      
      local comment = adapter:get_suppression_comment(diagnostic)
      assert.is_not_nil(comment)
      assert.is_true(comment:match("pyright") ~= nil or comment:match("type:.*ignore") ~= nil)
    end)

    it("generates ruff suppression comment", function()
      local diagnostic = {
        source = "ruff",
        code = "F401",
        message = "unused import"
      }
      
      local comment = adapter:get_suppression_comment(diagnostic)
      assert.is_not_nil(comment)
      assert.is_true(comment:match("noqa") ~= nil)
      assert.is_true(comment:match("F401") ~= nil)
    end)

    it("generates flake8 suppression comment", function()
      local diagnostic = {
        source = "flake8",
        code = "E501",
        message = "line too long"
      }
      
      local comment = adapter:get_suppression_comment(diagnostic)
      assert.is_not_nil(comment)
      assert.is_true(comment:match("noqa") ~= nil)
      assert.is_true(comment:match("E501") ~= nil)
    end)

    it("generates generic suppression comment for unknown source", function()
      local diagnostic = {
        source = "unknown-linter",
        code = "some-code",
        message = "some message"
      }
      
      local comment = adapter:get_suppression_comment(diagnostic)
      assert.is_not_nil(comment)
      -- Should fall back to a generic comment
      assert.is_true(comment:match("noqa") ~= nil or comment:match("type:.*ignore") ~= nil)
    end)
  end)

  describe("insert_suppression_comment", function()
    it("inserts inline comment at end of line", function()
      local lines = {
        "import os",
        "import sys",
        "print('hello')"
      }
      
      local diagnostic = {
        source = "pylint",
        code = "W0611",
        lnum = 1,  -- 0-indexed, so line 2
        col = 0
      }

      -- Mock vim.api
      _G.vim = _G.vim or {}
      _G.vim.api = _G.vim.api or {}
      local buffer_lines = vim.fn.deepcopy(lines)
      
      _G.vim.api.nvim_buf_get_lines = function(bufnr, start, end_, strict)
        return { buffer_lines[start + 1] }
      end
      
      _G.vim.api.nvim_buf_set_lines = function(bufnr, start, end_, strict, replacement)
        buffer_lines[start + 1] = replacement[1]
      end

      adapter:insert_suppression_comment(0, diagnostic, "inline")
      
      -- Should have appended comment to line 2
      assert.is_true(buffer_lines[2]:match("# pylint:") ~= nil or buffer_lines[2]:match("# noqa:") ~= nil)
    end)

    it("inserts comment on previous line for above scope", function()
      local lines = {
        "import os",
        "import sys",
        "print('hello')"
      }
      
      local diagnostic = {
        source = "mypy",
        code = "assignment",
        lnum = 1,
        col = 0
      }

      -- Mock vim.api
      _G.vim = _G.vim or {}
      _G.vim.api = _G.vim.api or {}
      local buffer_lines = vim.fn.deepcopy(lines)
      
      _G.vim.api.nvim_buf_get_lines = function(bufnr, start, end_, strict)
        return { buffer_lines[start + 1] }
      end
      
      _G.vim.api.nvim_buf_set_lines = function(bufnr, start, end_, strict, replacement)
        -- Insert new line
        table.insert(buffer_lines, start + 1, replacement[1])
      end

      adapter:insert_suppression_comment(0, diagnostic, "above")
      
      -- Should have inserted comment on line before diagnostic
      assert.is_true(buffer_lines[2]:match("# type:.*ignore") ~= nil)
    end)

    it("inserts file-level suppression at top of file", function()
      local lines = {
        "import os",
        "import sys",
        "print('hello')"
      }
      
      local diagnostic = {
        source = "pylint",
        code = "C0114",
        lnum = 0,
        col = 0
      }

      -- Mock vim.api
      _G.vim = _G.vim or {}
      _G.vim.api = _G.vim.api or {}
      local buffer_lines = vim.fn.deepcopy(lines)
      
      _G.vim.api.nvim_buf_set_lines = function(bufnr, start, end_, strict, replacement)
        -- Insert at beginning
        table.insert(buffer_lines, 1, replacement[1])
      end

      adapter:insert_suppression_comment(0, diagnostic, "file")
      
      -- Should have inserted at top of file
      assert.is_true(buffer_lines[1]:match("# pylint:") ~= nil)
    end)
  end)

  describe("supports_source", function()
    it("returns true for pylint", function()
      assert.is_true(adapter:supports_source("pylint"))
    end)

    it("returns true for mypy", function()
      assert.is_true(adapter:supports_source("mypy"))
    end)

    it("returns true for pyright", function()
      assert.is_true(adapter:supports_source("pyright"))
    end)

    it("returns true for ruff", function()
      assert.is_true(adapter:supports_source("ruff"))
    end)

    it("returns true for flake8", function()
      assert.is_true(adapter:supports_source("flake8"))
    end)

    it("returns false for non-Python sources", function()
      assert.is_false(adapter:supports_source("eslint"))
      assert.is_false(adapter:supports_source("typescript"))
    end)
  end)

  describe("get_filetype", function()
    it("returns python as supported filetype", function()
      assert.are.equal("python", adapter:get_filetype())
    end)
  end)
end)

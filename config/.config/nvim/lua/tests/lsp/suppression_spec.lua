-- ABOUTME: Tests for diagnostic suppression system
-- ABOUTME: Verifies storage, filtering, and scope detection for suppressed diagnostics

local Storage = require('lsp.features.diagnostics.storage')
local ScopeDetector = require('lsp.features.diagnostics.scope_detector')
local SuppressionManager = require('lsp.features.diagnostics.manager')

describe("Suppression Storage", function()
  local storage
  local test_config_path

  before_each(function()
    -- Use a temporary test config path
    test_config_path = vim.fn.tempname() .. ".json"
    storage = Storage:new(test_config_path)
  end)

  after_each(function()
    -- Clean up test file
    if vim.fn.filereadable(test_config_path) == 1 then
      vim.fn.delete(test_config_path)
    end
  end)

  it("can be instantiated", function()
    assert.is_not_nil(storage)
  end)

  describe("add_suppression", function()
    it("adds suppression to storage", function()
      storage:add_suppression({
        source = "pylint",
        code = "W0611",
        scope = "file",
        file = "test.py",
        line = 10
      })

      local suppressions = storage:get_suppressions("test.py")
      assert.are.equal(1, #suppressions)
      assert.are.equal("pylint", suppressions[1].source)
      assert.are.equal("W0611", suppressions[1].code)
    end)

    it("persists suppressions to disk", function()
      storage:add_suppression({
        source = "pylint",
        code = "W0611",
        scope = "file",
        file = "test.py"
      })

      -- Create new storage instance to verify persistence
      local new_storage = Storage:new(test_config_path)
      local suppressions = new_storage:get_suppressions("test.py")
      assert.are.equal(1, #suppressions)
    end)
  end)

  describe("remove_suppression", function()
    it("removes suppression from storage", function()
      storage:add_suppression({
        source = "pylint",
        code = "W0611",
        scope = "file",
        file = "test.py"
      })

      storage:remove_suppression({
        source = "pylint",
        code = "W0611",
        file = "test.py"
      })

      local suppressions = storage:get_suppressions("test.py")
      assert.are.equal(0, #suppressions)
    end)
  end)

  describe("get_suppressions", function()
    it("returns suppressions for specific file", function()
      storage:add_suppression({
        source = "pylint",
        code = "W0611",
        scope = "file",
        file = "test1.py"
      })
      storage:add_suppression({
        source = "mypy",
        code = "error",
        scope = "file",
        file = "test2.py"
      })

      local suppressions = storage:get_suppressions("test1.py")
      assert.are.equal(1, #suppressions)
      assert.are.equal("pylint", suppressions[1].source)
    end)

    it("returns empty list for file with no suppressions", function()
      local suppressions = storage:get_suppressions("nonexistent.py")
      assert.are.same({}, suppressions)
    end)
  end)

  describe("is_suppressed", function()
    before_each(function()
      storage:add_suppression({
        source = "pylint",
        code = "W0611",
        scope = "statement",
        file = "test.py",
        line = 10
      })
      storage:add_suppression({
        source = "mypy",
        code = "error",
        scope = "file",
        file = "test.py"
      })
    end)

    it("returns true for suppressed diagnostic at exact line", function()
      local diagnostic = {
        source = "pylint",
        code = "W0611",
        lnum = 10
      }

      assert.is_true(storage:is_suppressed("test.py", diagnostic))
    end)

    it("returns true for file-level suppression", function()
      local diagnostic = {
        source = "mypy",
        code = "error",
        lnum = 99
      }

      assert.is_true(storage:is_suppressed("test.py", diagnostic))
    end)

    it("returns false for non-suppressed diagnostic", function()
      local diagnostic = {
        source = "ruff",
        code = "E501",
        lnum = 10
      }

      assert.is_false(storage:is_suppressed("test.py", diagnostic))
    end)
  end)
end)

describe("Scope Detector", function()
  local detector
  local TreesitterProvider = require('lsp.providers.treesitter')

  before_each(function()
    detector = ScopeDetector:new(TreesitterProvider:new())
  end)

  it("can be instantiated", function()
    assert.is_not_nil(detector)
  end)

  describe("get_scope_range", function()
    it("returns range for statement scope", function()
      local mock_node = {
        type = function() return "expression_statement" end,
        range = function() return 10, 0, 10, 20 end
      }
      
      _G.vim = _G.vim or {}
      _G.vim.treesitter = _G.vim.treesitter or {}
      _G.vim.treesitter.get_node = function()
        return mock_node
      end

      local range = detector:get_scope_range(0, { 10, 5 }, "statement")
      assert.is_not_nil(range)
      assert.are.equal(10, range.start_line)
      assert.are.equal(10, range.end_line)
    end)

    it("returns nil when node not found", function()
      _G.vim = _G.vim or {}
      _G.vim.treesitter = _G.vim.treesitter or {}
      _G.vim.treesitter.get_node = function()
        return nil
      end

      local range = detector:get_scope_range(0, { 10, 5 }, "statement")
      assert.is_nil(range)
    end)
  end)

  describe("get_available_scopes", function()
    it("returns available scope levels for cursor position", function()
      -- Mock nested nodes: statement inside function inside class
      local statement_node = {
        type = function() return "expression_statement" end,
        range = function() return 10, 0, 10, 20 end,
        parent = function() return function_node end
      }
      
      local function_node = {
        type = function() return "function_definition" end,
        range = function() return 8, 0, 15, 0 end,
        parent = function() return class_node end
      }
      
      local class_node = {
        type = function() return "class_definition" end,
        range = function() return 5, 0, 20, 0 end,
        parent = function() return nil end
      }

      _G.vim = _G.vim or {}
      _G.vim.treesitter = _G.vim.treesitter or {}
      _G.vim.treesitter.get_node = function()
        return statement_node
      end

      local scopes = detector:get_available_scopes(0, { 10, 5 })
      
      -- Should return statement, function, class, and file
      assert.is_true(#scopes >= 2) -- At minimum statement and file
    end)
  end)
end)

describe("Suppression Manager", function()
  local manager
  local test_config_path

  before_each(function()
    test_config_path = vim.fn.tempname() .. ".json"
    
    local LspProvider = require('lsp.providers.lsp')
    local TreesitterProvider = require('lsp.providers.treesitter')
    local NoneLsProvider = require('lsp.providers.none_ls')

    manager = SuppressionManager:new({
      lsp_provider = LspProvider:new(),
      ts_provider = TreesitterProvider:new(),
      none_ls_provider = NoneLsProvider:new(),
      config_path = test_config_path
    })
  end)

  after_each(function()
    if vim.fn.filereadable(test_config_path) == 1 then
      vim.fn.delete(test_config_path)
    end
  end)

  it("can be instantiated", function()
    assert.is_not_nil(manager)
  end)

  describe("suppress_diagnostic", function()
    it("suppresses diagnostic at file scope", function()
      local diagnostic = {
        source = "pylint",
        code = "W0611",
        lnum = 10
      }

      local result = manager:suppress_diagnostic(0, "test.py", diagnostic, "file")
      assert.is_true(result)

      -- Verify it was stored
      assert.is_true(manager:is_diagnostic_suppressed("test.py", diagnostic))
    end)
  end)

  describe("filter_diagnostics", function()
    it("filters out suppressed diagnostics", function()
      local diagnostics = {
        { source = "pylint", code = "W0611", lnum = 10 },
        { source = "mypy", code = "error", lnum = 15 },
        { source = "ruff", code = "E501", lnum = 20 }
      }

      -- Suppress first diagnostic
      manager:suppress_diagnostic(0, "test.py", diagnostics[1], "file")

      local filtered = manager:filter_diagnostics("test.py", diagnostics)
      
      -- Should only have 2 diagnostics left
      assert.are.equal(2, #filtered)
      assert.are.equal("mypy", filtered[1].source)
      assert.are.equal("ruff", filtered[2].source)
    end)
  end)

  describe("get_diagnostic_at_cursor", function()
    it("returns diagnostic at cursor position", function()
      local diagnostics = {
        { source = "pylint", code = "W0611", lnum = 10, col = 5 },
        { source = "mypy", code = "error", lnum = 15, col = 10 }
      }

      _G.vim = _G.vim or {}
      _G.vim.diagnostic = _G.vim.diagnostic or {}
      _G.vim.diagnostic.get = function(bufnr)
        return diagnostics
      end

      local diagnostic = manager:get_diagnostic_at_cursor(0, { 10, 6 })
      assert.is_not_nil(diagnostic)
      assert.are.equal("pylint", diagnostic.source)
    end)

    it("returns nil when no diagnostic at cursor", function()
      _G.vim = _G.vim or {}
      _G.vim.diagnostic = _G.vim.diagnostic or {}
      _G.vim.diagnostic.get = function(bufnr)
        return {}
      end

      local diagnostic = manager:get_diagnostic_at_cursor(0, { 10, 5 })
      assert.is_nil(diagnostic)
    end)
  end)

  describe("adapter integration", function()
    local python_adapter
    local LspProvider
    local TreesitterProvider
    local NoneLsProvider

    before_each(function()
      LspProvider = require('lsp.providers.lsp')
      TreesitterProvider = require('lsp.providers.treesitter')
      NoneLsProvider = require('lsp.providers.none_ls')
      
      python_adapter = require('lsp.features.diagnostics.adapters.python'):new()
      manager = SuppressionManager:new({
        lsp_provider = LspProvider:new(),
        ts_provider = TreesitterProvider:new(),
        none_ls_provider = NoneLsProvider:new(),
        config_path = test_config_path,
        adapters = { python = python_adapter }
      })
    end)

    it("uses adapter to insert suppression comment", function()
      local diagnostic = {
        source = "pylint",
        code = "W0611",
        lnum = 10,
        col = 0
      }

      -- Mock vim.api
      _G.vim = _G.vim or {}
      _G.vim.api = _G.vim.api or {}
      local buffer_lines = { "import os", "import sys", "print('hello')" }
      
      _G.vim.api.nvim_buf_get_lines = function(bufnr, start, end_, strict)
        return { buffer_lines[start + 1] }
      end
      
      _G.vim.api.nvim_buf_get_option = function(bufnr, option)
        if option == "filetype" then
          return "python"
        end
      end
      
      local set_lines_called = false
      _G.vim.api.nvim_buf_set_lines = function(bufnr, start, end_, strict, replacement)
        buffer_lines[start + 1] = replacement[1]
        set_lines_called = true
      end

      -- Suppress with comment
      manager:suppress_with_comment(0, "test.py", diagnostic, "inline")
      
      -- Should have inserted comment
      assert.is_true(set_lines_called)
    end)

    it("returns nil when no adapter for filetype", function()
      local LspProvider = require('lsp.providers.lsp')
      local TreesitterProvider = require('lsp.providers.treesitter')
      local NoneLsProvider = require('lsp.providers.none_ls')
      
      -- Create manager without adapters
      local manager_no_adapter = SuppressionManager:new({
        lsp_provider = LspProvider:new(),
        ts_provider = TreesitterProvider:new(),
        none_ls_provider = NoneLsProvider:new(),
        config_path = test_config_path
      })

      -- Mock vim.api
      _G.vim = _G.vim or {}
      _G.vim.api = _G.vim.api or {}
      _G.vim.api.nvim_buf_get_option = function(bufnr, option)
        if option == "filetype" then
          return "javascript"
        end
      end

      local diagnostic = {
        source = "eslint",
        code = "no-unused-vars",
        lnum = 10
      }

      local result = manager_no_adapter:suppress_with_comment(0, "test.js", diagnostic, "inline")
      assert.is_false(result)
    end)
  end)
end)

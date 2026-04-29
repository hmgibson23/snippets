-- ABOUTME: Tests for LSP provider abstraction layer
-- ABOUTME: Verifies provider interface and concrete implementations

local Provider = require('lsp.providers')
local LspProvider = require('lsp.providers.lsp')
local TreesitterProvider = require('lsp.providers.treesitter')
local NoneLsProvider = require('lsp.providers.none_ls')

describe("Provider base class", function()
  it("can be instantiated", function()
    local provider = Provider:new()
    assert.is_not_nil(provider)
  end)

  it("has required interface methods", function()
    local provider = Provider:new()
    assert.is_function(provider.get_diagnostics)
    assert.is_function(provider.get_code_actions)
    assert.is_function(provider.get_scope_node)
    assert.is_function(provider.can_suppress)
    assert.is_function(provider.suppress)
    assert.is_function(provider.is_available)
  end)

  it("can be subclassed", function()
    local CustomProvider = Provider:new()
    function CustomProvider:get_diagnostics(bufnr)
      return { "custom" }
    end

    local instance = CustomProvider:new()
    assert.are.same({ "custom" }, instance:get_diagnostics(0))
  end)
end)

describe("LspProvider", function()
  local provider

  before_each(function()
    provider = LspProvider:new()
  end)

  it("can be instantiated", function()
    assert.is_not_nil(provider)
  end)

  it("inherits from Provider", function()
    assert.is_function(provider.get_diagnostics)
    assert.is_function(provider.get_code_actions)
    assert.is_function(provider.get_scope_node)
    assert.is_function(provider.can_suppress)
    assert.is_function(provider.suppress)
    assert.is_function(provider.is_available)
  end)

  describe("get_diagnostics", function()
    it("returns diagnostics for buffer", function()
      -- Mock vim.diagnostic.get
      local mock_diagnostics = {
        { lnum = 10, col = 5, message = "test error", severity = 1 }
      }
      _G.vim = _G.vim or {}
      _G.vim.diagnostic = _G.vim.diagnostic or {}
      _G.vim.diagnostic.get = function(bufnr, opts)
        assert.are.equal(0, bufnr)
        return mock_diagnostics
      end

      local result = provider:get_diagnostics(0)
      assert.are.same(mock_diagnostics, result)
    end)
  end)

  describe("is_available", function()
    it("returns true when LSP client is attached", function()
      _G.vim = _G.vim or {}
      _G.vim.lsp = _G.vim.lsp or {}
      _G.vim.lsp.get_clients = function(opts)
        assert.are.equal(0, opts.bufnr)
        return { { name = "pylsp" } }
      end

      assert.is_true(provider:is_available(0))
    end)

    it("returns false when no LSP client is attached", function()
      _G.vim = _G.vim or {}
      _G.vim.lsp = _G.vim.lsp or {}
      _G.vim.lsp.get_clients = function(opts)
        return {}
      end

      assert.is_false(provider:is_available(0))
    end)
  end)

  describe("can_suppress", function()
    it("returns true for diagnostics with source", function()
      local diagnostic = { source = "pylint", code = "W0611" }
      assert.is_true(provider:can_suppress(diagnostic))
    end)

    it("returns false for diagnostics without source", function()
      local diagnostic = { message = "error" }
      assert.is_false(provider:can_suppress(diagnostic))
    end)
  end)
end)

describe("TreesitterProvider", function()
  local provider

  before_each(function()
    provider = TreesitterProvider:new()
  end)

  it("can be instantiated", function()
    assert.is_not_nil(provider)
  end)

  it("inherits from Provider", function()
    assert.is_function(provider.get_diagnostics)
    assert.is_function(provider.get_code_actions)
    assert.is_function(provider.get_scope_node)
    assert.is_function(provider.can_suppress)
    assert.is_function(provider.suppress)
    assert.is_function(provider.is_available)
  end)

  describe("is_available", function()
    it("returns true when treesitter parser is available", function()
      _G.vim = _G.vim or {}
      _G.vim.treesitter = _G.vim.treesitter or {}
      _G.vim.treesitter.get_parser = function(bufnr, lang)
        assert.are.equal(0, bufnr)
        return { lang = function() return "python" end }
      end

      assert.is_true(provider:is_available(0))
    end)

    it("returns false when treesitter parser is not available", function()
      _G.vim = _G.vim or {}
      _G.vim.treesitter = _G.vim.treesitter or {}
      _G.vim.treesitter.get_parser = function(bufnr, lang)
        error("No parser available")
      end

      assert.is_false(provider:is_available(0))
    end)
  end)

  describe("get_scope_node", function()
    it("returns node for statement scope", function()
      local mock_node = { type = function() return "expression_statement" end }
      _G.vim = _G.vim or {}
      _G.vim.treesitter = _G.vim.treesitter or {}
      _G.vim.treesitter.get_node = function()
        return mock_node
      end

      local result = provider:get_scope_node(0, { 10, 5 }, "statement")
      assert.is_not_nil(result)
    end)

    it("returns node for function scope", function()
      local mock_node = { type = function() return "function_definition" end }
      _G.vim = _G.vim or {}
      _G.vim.treesitter = _G.vim.treesitter or {}
      _G.vim.treesitter.get_node = function()
        return {
          parent = function() return mock_node end,
          type = function() return "block" end
        }
      end

      local result = provider:get_scope_node(0, { 10, 5 }, "function")
      -- Will need to walk up tree to find function node
      assert.is_not_nil(result)
    end)
  end)

  describe("get_diagnostics", function()
    it("returns empty list (treesitter doesn't provide diagnostics)", function()
      local result = provider:get_diagnostics(0)
      assert.are.same({}, result)
    end)
  end)

  describe("can_suppress", function()
    it("returns false (treesitter doesn't support suppression)", function()
      local diagnostic = { source = "pylint", code = "W0611" }
      assert.is_false(provider:can_suppress(diagnostic))
    end)
  end)
end)

describe("NoneLsProvider", function()
  local provider

  before_each(function()
    provider = NoneLsProvider:new()
  end)

  it("can be instantiated", function()
    assert.is_not_nil(provider)
  end)

  it("inherits from Provider", function()
    assert.is_function(provider.get_diagnostics)
    assert.is_function(provider.get_code_actions)
    assert.is_function(provider.get_scope_node)
    assert.is_function(provider.can_suppress)
    assert.is_function(provider.suppress)
    assert.is_function(provider.is_available)
  end)

  describe("is_available", function()
    it("returns true when none-ls is loaded", function()
      -- Mock package.loaded
      package.loaded['null-ls'] = { builtins = {} }

      assert.is_true(provider:is_available(0))

      -- Clean up
      package.loaded['null-ls'] = nil
    end)

    it("returns false when none-ls is not loaded", function()
      package.loaded['null-ls'] = nil

      assert.is_false(provider:is_available(0))
    end)
  end)

  describe("get_diagnostics", function()
    it("returns diagnostics from none-ls sources", function()
      -- Mock vim.diagnostic.get to filter by none-ls source
      local all_diagnostics = {
        { lnum = 10, col = 5, message = "test error", severity = 1, source = "pylint" },
        { lnum = 15, col = 2, message = "other error", severity = 1, source = "pyright" },
      }
      _G.vim = _G.vim or {}
      _G.vim.diagnostic = _G.vim.diagnostic or {}
      _G.vim.diagnostic.get = function(bufnr, opts)
        return all_diagnostics
      end

      -- Mock none-ls to indicate it has pylint registered
      package.loaded['null-ls'] = {
        builtins = {
          diagnostics = {
            pylint = { name = "pylint" }
          }
        }
      }

      local result = provider:get_diagnostics(0)
      
      -- Should only return diagnostics from none-ls sources
      assert.are.equal(1, #result)
      assert.are.equal("pylint", result[1].source)

      -- Clean up
      package.loaded['null-ls'] = nil
    end)
  end)

  describe("can_suppress", function()
    it("returns true for diagnostics from none-ls sources", function()
      package.loaded['null-ls'] = {
        builtins = {
          diagnostics = {
            pylint = { name = "pylint" }
          }
        }
      }

      local diagnostic = { source = "pylint", code = "W0611" }
      assert.is_true(provider:can_suppress(diagnostic))

      package.loaded['null-ls'] = nil
    end)

    it("returns false for diagnostics not from none-ls", function()
      package.loaded['null-ls'] = {
        builtins = {
          diagnostics = {}
        }
      }

      local diagnostic = { source = "pyright", code = "error" }
      assert.is_false(provider:can_suppress(diagnostic))

      package.loaded['null-ls'] = nil
    end)
  end)
end)

-- ABOUTME: Tests for DAP Python interpreter resolution
-- ABOUTME: Validates uv detection, venv integration, and fallback behavior

local interpreter = require('dap.python.interpreter')

describe('interpreter', function()
  describe('resolve', function()
    it('should return uv marker for uv projects', function()
      local context = {
        is_uv_project = true,
        project_root = '/home/user/project',
      }
      
      local result = interpreter.resolve(context)
      assert.are.equal('uv', result)
    end)
    
    it('should use venv-selector for non-uv projects with active venv', function()
      local context = {
        is_uv_project = false,
        project_root = nil,
      }
      
      -- Mock venv-selector
      package.loaded['venv-selector'] = {
        venv = function()
          return '/home/user/.venv/bin/python'
        end
      }
      
      local result = interpreter.resolve(context)
      assert.are.equal('/home/user/.venv/bin/python', result)
      
      -- Clean up
      package.loaded['venv-selector'] = nil
    end)
    
    it('should fall back to python3 when no venv is active', function()
      local context = {
        is_uv_project = false,
        project_root = nil,
      }
      
      -- Mock venv-selector to return nil
      package.loaded['venv-selector'] = {
        venv = function()
          return nil
        end
      }
      
      -- Mock vim.fn.executable
      local original_executable = vim.fn.executable
      vim.fn.executable = function(cmd)
        if cmd == 'python3' then
          return 1
        end
        return 0
      end
      
      local result = interpreter.resolve(context)
      assert.are.equal('python3', result)
      
      -- Clean up
      package.loaded['venv-selector'] = nil
      vim.fn.executable = original_executable
    end)
    
    it('should fall back to python when python3 is not available', function()
      local context = {
        is_uv_project = false,
        project_root = nil,
      }
      
      -- Mock venv-selector not being loaded
      package.loaded['venv-selector'] = nil
      
      -- Mock vim.fn.executable
      local original_executable = vim.fn.executable
      vim.fn.executable = function(cmd)
        if cmd == 'python' then
          return 1
        end
        return 0
      end
      
      local result = interpreter.resolve(context)
      assert.are.equal('python', result)
      
      -- Clean up
      vim.fn.executable = original_executable
    end)
    
    it('should return nil when no Python interpreter is found', function()
      local context = {
        is_uv_project = false,
        project_root = nil,
      }
      
      package.loaded['venv-selector'] = nil
      
      local original_executable = vim.fn.executable
      vim.fn.executable = function(cmd)
        return 0
      end
      
      local result = interpreter.resolve(context)
      assert.is_nil(result)
      
      vim.fn.executable = original_executable
    end)
  end)
end)

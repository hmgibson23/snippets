-- ABOUTME: Tests for DAP Python launcher configuration builder
-- ABOUTME: Validates script and FastAPI server configuration generation

local launcher = require('dap.python.launcher')

describe('launcher', function()
  describe('find_fastapi_entry', function()
    it('should find main.py with FastAPI app', function()
      local project_root = '/home/user/project'
      
      -- Mock file system
      local original_fs_stat = vim.loop.fs_stat
      vim.loop.fs_stat = function(path)
        if path == '/home/user/project/main.py' then
          return { type = 'file' }
        end
        return nil
      end
      
      -- Mock file reading
      local original_readfile = vim.fn.readfile
      vim.fn.readfile = function(path)
        if path == '/home/user/project/main.py' then
          return {
            'from fastapi import FastAPI',
            '',
            'app = FastAPI()',
            '',
            '@app.get("/")',
            'def root():',
            '    return {"message": "Hello"}',
          }
        end
        return {}
      end
      
      local result = launcher.find_fastapi_entry(project_root)
      assert.are.equal('main:app', result)
      
      -- Clean up
      vim.loop.fs_stat = original_fs_stat
      vim.fn.readfile = original_readfile
    end)
    
    it('should find app.py with custom app variable name', function()
      local project_root = '/home/user/project'
      
      local original_fs_stat = vim.loop.fs_stat
      vim.loop.fs_stat = function(path)
        if path == '/home/user/project/app.py' then
          return { type = 'file' }
        end
        return nil
      end
      
      local original_readfile = vim.fn.readfile
      vim.fn.readfile = function(path)
        if path == '/home/user/project/app.py' then
          return {
            'from fastapi import FastAPI',
            '',
            'application = FastAPI()',
          }
        end
        return {}
      end
      
      local result = launcher.find_fastapi_entry(project_root)
      assert.are.equal('app:application', result)
      
      vim.loop.fs_stat = original_fs_stat
      vim.fn.readfile = original_readfile
    end)
    
    it('should return nil when no entry point is found', function()
      local project_root = '/home/user/project'
      
      local original_fs_stat = vim.loop.fs_stat
      vim.loop.fs_stat = function(path)
        return nil
      end
      
      local result = launcher.find_fastapi_entry(project_root)
      assert.is_nil(result)
      
      vim.loop.fs_stat = original_fs_stat
    end)
  end)
  
  describe('build_config', function()
    it('should build script config for uv projects', function()
      local context = {
        filepath = '/home/user/project/script.py',
        project_root = '/home/user/project',
        file_type = 'script',
        is_uv_project = true,
      }
      local interpreter = 'uv'
      
      local config = launcher.build_config(context, interpreter)
      
      assert.are.equal('python', config.type)
      assert.are.equal('launch', config.request)
      assert.are.equal('/home/user/project/script.py', config.program)
      assert.are.equal('/home/user/project', config.cwd)
      assert.is_false(config.justMyCode)
      assert.is_nil(config.pythonPath) -- uv projects don't set pythonPath
      assert.are.same({ 'uv', 'run', 'python' }, config.python)
    end)
    
    it('should build script config for non-uv projects', function()
      local context = {
        filepath = '/home/user/script.py',
        project_root = nil,
        file_type = 'script',
        is_uv_project = false,
      }
      local interpreter = '/home/user/.venv/bin/python'
      
      local config = launcher.build_config(context, interpreter)
      
      assert.are.equal('python', config.type)
      assert.are.equal('launch', config.request)
      assert.are.equal('/home/user/script.py', config.program)
      assert.are.equal('/home/user/.venv/bin/python', config.pythonPath)
      assert.is_nil(config.python)
    end)
    
    it('should build FastAPI config for uv projects', function()
      local context = {
        filepath = '/home/user/project/main.py',
        project_root = '/home/user/project',
        file_type = 'fastapi',
        is_uv_project = true,
      }
      local interpreter = 'uv'
      
      -- Mock find_fastapi_entry
      local original_find = launcher.find_fastapi_entry
      launcher.find_fastapi_entry = function(root)
        return 'main:app'
      end
      
      local config = launcher.build_config(context, interpreter)
      
      assert.are.equal('python', config.type)
      assert.are.equal('launch', config.request)
      assert.are.equal('uvicorn', config.module)
      assert.are.same({ 'main:app', '--reload' }, config.args)
      assert.are.equal('/home/user/project', config.cwd)
      assert.is_false(config.justMyCode)
      assert.are.same({ 'uv', 'run', 'python' }, config.python)
      
      -- Clean up
      launcher.find_fastapi_entry = original_find
    end)
    
    it('should build FastAPI config for non-uv projects', function()
      local context = {
        filepath = '/home/user/project/main.py',
        project_root = '/home/user/project',
        file_type = 'fastapi',
        is_uv_project = false,
      }
      local interpreter = 'python3'
      
      local original_find = launcher.find_fastapi_entry
      launcher.find_fastapi_entry = function(root)
        return 'main:app'
      end
      
      local config = launcher.build_config(context, interpreter)
      
      assert.are.equal('python', config.type)
      assert.are.equal('uvicorn', config.module)
      assert.are.same({ 'main:app', '--reload' }, config.args)
      assert.are.equal('python3', config.pythonPath)
      assert.is_nil(config.python)
      
      launcher.find_fastapi_entry = original_find
    end)
  end)
end)

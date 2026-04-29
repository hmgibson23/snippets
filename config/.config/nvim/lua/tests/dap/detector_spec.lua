-- ABOUTME: Tests for DAP Python project detection functionality
-- ABOUTME: Validates pyproject.toml finding, FastAPI pattern detection, and file type classification

local detector = require('dap.python.detector')

describe('detector', function()
  describe('find_project_root', function()
    it('should find pyproject.toml in current directory', function()
      -- Create a mock file system state
      local mock_fs = {
        ['/home/user/project/pyproject.toml'] = true,
        ['/home/user/project/main.py'] = true,
      }
      
      -- Mock vim.loop.fs_stat
      local original_fs_stat = vim.loop.fs_stat
      vim.loop.fs_stat = function(path)
        if mock_fs[path] then
          return { type = 'file' }
        end
        return nil
      end
      
      local result = detector.find_project_root('/home/user/project/main.py')
      assert.equals('/home/user/project', result)
      
      -- Restore
      vim.loop.fs_stat = original_fs_stat
    end)
    
    it('should find pyproject.toml in parent directory', function()
      local mock_fs = {
        ['/home/user/project/pyproject.toml'] = true,
        ['/home/user/project/src/script.py'] = true,
      }
      
      local original_fs_stat = vim.loop.fs_stat
      vim.loop.fs_stat = function(path)
        if mock_fs[path] then
          return { type = 'file' }
        end
        return nil
      end
      
      local result = detector.find_project_root('/home/user/project/src/script.py')
      assert.equals('/home/user/project', result)
      
      vim.loop.fs_stat = original_fs_stat
    end)
    
    it('should return nil when no pyproject.toml found', function()
      local original_fs_stat = vim.loop.fs_stat
      vim.loop.fs_stat = function(path)
        return nil
      end
      
      local result = detector.find_project_root('/home/user/script.py')
      assert.is_nil(result)
      
      vim.loop.fs_stat = original_fs_stat
    end)
  end)
  
  describe('detect_file_type', function()
    it('should detect FastAPI import pattern', function()
      local content = [[
from fastapi import FastAPI

app = FastAPI()

@app.get("/")
def read_root():
    return {"Hello": "World"}
]]
      local result = detector.detect_file_type(content)
      assert.equals('fastapi', result)
    end)
    
    it('should detect uvicorn pattern', function()
      local content = [[
import uvicorn

if __name__ == "__main__":
    uvicorn.run("main:app", host="0.0.0.0", port=8000)
]]
      local result = detector.detect_file_type(content)
      assert.equals('fastapi', result)
    end)
    
    it('should detect FastAPI app instantiation', function()
      local content = [[
from fastapi import FastAPI

application = FastAPI()
]]
      local result = detector.detect_file_type(content)
      assert.equals('fastapi', result)
    end)
    
    it('should return script for regular Python files', function()
      local content = [[
def process_data(input_file):
    with open(input_file) as f:
        data = f.read()
    return data

if __name__ == "__main__":
    process_data("input.txt")
]]
      local result = detector.detect_file_type(content)
      assert.equals('script', result)
    end)
    
    it('should return script for empty content', function()
      local result = detector.detect_file_type('')
      assert.equals('script', result)
    end)
  end)
  
  describe('detect_context', function()
    after_each(function()
      detector.clear_all_caches()
    end)
    
    it('should detect uv project with FastAPI', function()
      local mock_bufnr = 1
      local mock_content = [[
from fastapi import FastAPI
app = FastAPI()
]]
      
      -- Mock buffer content
      local original_get_lines = vim.api.nvim_buf_get_lines
      vim.api.nvim_buf_get_lines = function(bufnr, start, end_, strict)
        if bufnr == mock_bufnr then
          return vim.split(mock_content, '\n')
        end
        return {}
      end
      
      -- Mock file path
      local original_buf_get_name = vim.api.nvim_buf_get_name
      vim.api.nvim_buf_get_name = function(bufnr)
        if bufnr == mock_bufnr then
          return '/home/user/project/main.py'
        end
        return ''
      end
      
      -- Mock fs_stat to find pyproject.toml
      local original_fs_stat = vim.loop.fs_stat
      vim.loop.fs_stat = function(path)
        if path == '/home/user/project/pyproject.toml' then
          return { type = 'file' }
        end
        return nil
      end
      
      local result = detector.detect_context(mock_bufnr)
      
      assert.equals('/home/user/project', result.project_root)
      assert.equals('fastapi', result.file_type)
      assert.is_true(result.is_uv_project)
      
      -- Restore
      vim.api.nvim_buf_get_lines = original_get_lines
      vim.api.nvim_buf_get_name = original_buf_get_name
      vim.loop.fs_stat = original_fs_stat
    end)
    
    it('should detect standalone script without uv', function()
      local mock_bufnr = 1
      local mock_content = [[
def main():
    print("Hello")

if __name__ == "__main__":
    main()
]]
      
      local original_get_lines = vim.api.nvim_buf_get_lines
      vim.api.nvim_buf_get_lines = function(bufnr, start, end_, strict)
        if bufnr == mock_bufnr then
          return vim.split(mock_content, '\n')
        end
        return {}
      end
      
      local original_buf_get_name = vim.api.nvim_buf_get_name
      vim.api.nvim_buf_get_name = function(bufnr)
        if bufnr == mock_bufnr then
          return '/home/user/script.py'
        end
        return ''
      end
      
      local original_fs_stat = vim.loop.fs_stat
      vim.loop.fs_stat = function(path)
        return nil
      end
      
      local result = detector.detect_context(mock_bufnr)
      
      assert.is_nil(result.project_root)
      assert.equals('script', result.file_type)
      assert.is_false(result.is_uv_project)
      
      vim.api.nvim_buf_get_lines = original_get_lines
      vim.api.nvim_buf_get_name = original_buf_get_name
      vim.loop.fs_stat = original_fs_stat
    end)
  end)
end)

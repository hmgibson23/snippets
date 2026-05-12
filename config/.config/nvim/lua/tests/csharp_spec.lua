local csharp = require('csharp')

describe('csharp helpers', function()
  it('detects solution and project files under a root', function()
    local original_glob = vim.fn.glob
    vim.fn.glob = function(pattern, _, _)
      if pattern:match('%*%.slnx$') then
        return { '/repo/App.slnx' }
      end
      if pattern:match('%*%.sln$') then
        return { '/repo/Legacy.sln' }
      end
      if pattern:match('%*%.csproj$') then
        return { '/repo/src/App/App.csproj' }
      end
      return {}
    end

    local files = csharp.project_files('/repo')

    assert.are.same({ '/repo/App.slnx', '/repo/Legacy.sln', '/repo/src/App/App.csproj' }, files)
    vim.fn.glob = original_glob
  end)

  it('prefers csharpier executable for formatting', function()
    local original_executable = vim.fn.executable
    vim.fn.executable = function(cmd)
      return cmd == 'csharpier' and 1 or 0
    end

    assert.are.equal('csharpier .', csharp.command('format', '/repo'))
    vim.fn.executable = original_executable
  end)

  it('falls back to dotnet format when csharpier is unavailable', function()
    local original_executable = vim.fn.executable
    vim.fn.executable = function(cmd)
      return cmd == 'dotnet' and 1 or 0
    end

    assert.are.equal('dotnet format', csharp.command('format', '/repo'))
    vim.fn.executable = original_executable
  end)

  it('exposes core dotnet workflow commands', function()
    assert.are.equal('dotnet build', csharp.command('build', '/repo'))
    assert.are.equal('dotnet test', csharp.command('test', '/repo'))
    assert.are.equal('dotnet restore', csharp.command('restore', '/repo'))
    assert.are.equal('dotnet clean', csharp.command('clean', '/repo'))
    assert.are.equal('dotnet watch test', csharp.command('watch_test', '/repo'))
  end)
end)

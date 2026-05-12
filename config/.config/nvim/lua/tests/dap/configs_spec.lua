local configs = require('dap.configs')

describe('dap.configs', function()
  it('builds useful JS/TS launch and attach configurations', function()
    local js = configs.javascript()

    assert.is_true(#js >= 4)
    assert.are.equal('pwa-node', js[1].type)
    assert.are.equal('launch', js[1].request)
    assert.are.equal('${file}', js[1].program)
    assert.are.equal('${workspaceFolder}', js[1].cwd)

    local has_attach = false
    local has_browser = false
    for _, cfg in ipairs(js) do
      if cfg.request == 'attach' and cfg.processId then
        has_attach = true
      end
      if cfg.type == 'pwa-chrome' then
        has_browser = true
      end
    end

    assert.is_true(has_attach)
    assert.is_true(has_browser)
  end)

  it('builds prompt-driven C/C++ configurations', function()
    local cpp = configs.cpp()

    assert.is_true(#cpp >= 2)
    assert.are.equal('cppdbg', cpp[1].type)
    assert.are.equal('launch', cpp[1].request)
    assert.are.equal('${workspaceFolder}', cpp[1].cwd)
    assert.are.equal('function', type(cpp[1].program))
  end)

  it('registers configurations for all supported filetypes', function()
    local dap = { configurations = {}, adapters = {} }
    configs.apply(dap)

    assert.is_not_nil(dap.configurations.python)
    assert.is_not_nil(dap.configurations.javascript)
    assert.is_not_nil(dap.configurations.typescript)
    assert.is_not_nil(dap.configurations.c)
    assert.is_not_nil(dap.configurations.cpp)
    assert.is_not_nil(dap.configurations.go)
    assert.is_not_nil(dap.adapters.cppdbg)
  end)
end)

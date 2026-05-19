describe("workflow-local lazy spec", function()
  local function contains(list, value)
    for _, item in ipairs(list or {}) do
      if item == value then
        return true
      end
    end
    return false
  end

  local function has_key(keys, lhs)
    for _, key in ipairs(keys or {}) do
      if key[1] == lhs then
        return true, key
      end
    end
    return false
  end

  it("declares command triggers for health and diagnostic suppression commands", function()
    local plugin = require("plugins.workflows")

    assert.is_true(contains(plugin.cmd, "ConfigHealth"))
    assert.is_true(contains(plugin.cmd, "ConfigProfile"))
    assert.is_true(contains(plugin.cmd, "ConfigReload"))
    assert.is_true(contains(plugin.cmd, "SuppressDiagnostic"))
    assert.is_true(contains(plugin.cmd, "SuppressDiagnosticAbove"))
    assert.is_true(contains(plugin.cmd, "SuppressDiagnosticFile"))
    assert.is_true(contains(plugin.cmd, "UnsuppressDiagnostic"))
  end)

  it("declares suppression keys without using diagnostic summary on suppress inline", function()
    local plugin = require("plugins.workflows")

    local has_inline, inline = has_key(plugin.keys, "<leader>lxs")
    assert.is_true(has_inline)
    assert.are_not.equal("Diagnostic summary", inline.desc)

    assert.is_true(has_key(plugin.keys, "<leader>lxS"))
    assert.is_true(has_key(plugin.keys, "<leader>lxf"))
    assert.is_true(has_key(plugin.keys, "<leader>lxu"))
    assert.is_true(has_key(plugin.keys, "<leader>lxi"))
    assert.is_true(has_key(plugin.keys, "<leader>lxl"))
    assert.is_true(has_key(plugin.keys, "<leader>lC"))
  end)
end)

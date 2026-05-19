describe("hlslens Colemak search bindings", function()
  local function has_key(keys, lhs)
    for _, key in ipairs(keys or {}) do
      if key[1] == lhs then
        return true, key
      end
    end
    return false
  end

  it("keeps k/K as Colemak search next/previous lazy triggers", function()
    local plugin = require("plugins.hslens")

    local has_next, next_key = has_key(plugin.keys, "k")
    assert.is_true(has_next)
    assert.are.equal("Search next with lens", next_key.desc)

    local has_prev, prev_key = has_key(plugin.keys, "K")
    assert.is_true(has_prev)
    assert.are.equal("Search previous with lens", prev_key.desc)
  end)

  it("does not reclaim the global undo prefix for clearing search", function()
    local plugin = require("plugins.hslens")

    assert.is_false(has_key(plugin.keys, "<leader>u"))
    assert.is_true(has_key(plugin.keys, "<leader>hc"))
  end)
end)

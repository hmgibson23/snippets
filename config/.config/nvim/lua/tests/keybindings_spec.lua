describe("global Colemak keybindings", function()
  local function normal_map(lhs)
    for _, map in ipairs(vim.api.nvim_get_keymap("n")) do
      if map.lhs == lhs then
        return map
      end
    end
  end

  it("resizes windows in larger 5-cell steps", function()
    assert.are.equal("<C-W>5<lt>", normal_map("<C-W><lt>").rhs)
    assert.are.equal("<C-W>5>", normal_map("<C-W>>").rhs)
    assert.are.equal("<C-W>5+", normal_map("<C-W>+").rhs)
    assert.are.equal("<C-W>5-", normal_map("<C-W>-").rhs)
  end)

  it("keeps window navigation on Colemak directions", function()
    assert.are.equal("<C-W>h", normal_map("<C-W>h").rhs)
    assert.are.equal("<C-W>j", normal_map("<C-W>n").rhs)
    assert.are.equal("<C-W>k", normal_map("<C-W>e").rhs)
    assert.are.equal("<C-W>l", normal_map("<C-W>i").rhs)
  end)
end)

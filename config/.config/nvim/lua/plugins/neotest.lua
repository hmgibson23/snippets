local function adapter(name, setup)
  local ok, mod = pcall(require, name)
  if not ok then
    return nil
  end
  if setup then
    return mod(setup)
  end
  if type(mod) == "function" then
    return mod()
  end
  return mod
end

return {
  "nvim-neotest/neotest",
  lazy = true,
  dependencies = {
    {
      "vim-test/vim-test",
      cmd = { "TestLast", "TestSuite", "TestNearest", "TestFile" },
      init = function()
        vim.g["test#strategy"] = "neovim"
        vim.g["test#neovim#term_position"] = "belowright"
        vim.g["test#neovim#preserve_screen"] = 1
        vim.g["test#python#runner"] = "pytest"
        vim.g["test#javascript#reactscripts#options"] = "--watchAll=false"
        vim.g["test#javascript#runner"] = "jest"
        vim.g["test#javascript#cypress#executable"] = "npx cypress run-ct"
      end,
    },
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
    "nvim-neotest/neotest-vim-test",
    "nvim-neotest/neotest-python",
    "nvim-neotest/neotest-plenary",
    "nvim-neotest/neotest-go",
    "haydenmeade/neotest-jest",
    "rouge8/neotest-rust",
  },
  config = function()
    local adapters = {}
    for _, candidate in ipairs({
      adapter("neotest-python", { dap = { justMyCode = false } }),
      adapter("neotest-plenary"),
      adapter("neotest-go"),
      adapter("neotest-jest"),
      adapter("neotest-rust"),
      adapter("neotest-vim-test"),
    }) do
      if candidate then
        table.insert(adapters, candidate)
      end
    end

    require("neotest").setup({ adapters = adapters })
  end,
}

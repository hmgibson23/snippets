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
  keys = {
    { "<leader>k", group = "[K]Tests" },
    { "<leader>kn", function() require("neotest").run.run() end, desc = "Test nearest" },
    { "<leader>kf", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Test file" },
    { "<leader>ka", function() require("neotest").run.run({ suite = true }) end, desc = "Test suite" },
    { "<leader>kl", function() require("neotest").run.run_last() end, desc = "Run last" },
    { "<leader>kd", function() require("neotest").run.run({ strategy = "dap" }) end, desc = "Debug nearest" },
    { "<leader>kD", function() require("neotest").run.run({ vim.fn.expand("%"), strategy = "dap" }) end, desc = "Debug file" },
    { "<leader>ko", function() require("neotest").output.open({ enter = true }) end, desc = "Output" },
    { "<leader>kO", function() require("neotest").output_panel.toggle() end, desc = "Output panel" },
    { "<leader>ks", function() require("neotest").summary.toggle() end, desc = "Summary" },
    { "<leader>kS", function() require("neotest").run.stop() end, desc = "Stop" },
  },
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

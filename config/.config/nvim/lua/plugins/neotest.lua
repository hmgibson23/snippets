return {
  "nvim-neotest/neotest",
  dependencies = {
    {
      "vim-test/vim-test",
      cmd = {
        "TestLast",
        "TestSuite",
        "TestNearest",
        "TestFile",
      },
      config = function()
        local function javascript_runner()
          local runners = { "cypress", "jest" }
          vim.ui.select(runners, { prompt = "Choose Javascript Runner" }, function(selected)
            if selected then
              vim.g["test#javascript#runner"] = selected
              require("utils").info("Test runner changed to " .. selected, "Test Runner")
            end
          end)
        end
        local whichkey = require("which-key")
        whichkey.add({

          { "<leader>k",  group = "Neotest" },
          { "<leader>ka", "<cmd>lua require('neotest').run.attach()<cr>", desc = "Attach" },
          {
            "<leader>kA",
            "<cmd>lua require('neotest').run.run({ suite = true })<cr>",
            desc = "Run all",
          },
          {
            "<leader>kf",
            "<cmd>lua require('neotest').run.run(vim.fn.expand('%'))<cr>",
            desc = "Run file",
          },
          {
            "<leader>kF",
            "<cmd>lua require('neotest').run.run({vim.fn.expand('%'), strategy = 'dap'})<cr>",
            desc = "Debug File",
          },
          { "<leader>kl", "<cmd>lua require('neotest').run.run_last()<cr>",        desc = "Run last" },
          {
            "<leader>kN",
            "<cmd>lua require('neotest').run.run({strategy = 'dap'})<cr>",
            desc = "Debug nearest",
          },
          {
            "<leader>kO",
            "<cmd>lua require('neotest').output.open({ enter = true })<cr>",
            desc = "Output float",
          },
          { "<leader>ko", "<cmd>lua require('neotest').output_panel.toggle()<cr>", desc = "Output Panel" },
          { "<leader>kS", "<cmd>lua require('neotest').run.stop()<cr>",            desc = "Stop" },
          { "<leader>ks", "<cmd>lua require('neotest').summary.toggle()<cr>",      desc = "Summary" },
        })
        vim.g["test#strategy"] = "neovim"
        vim.g["test#neovim#term_position"] = "belowright"
        vim.g["test#neovim#preserve_screen"] = 1

        -- Python
        vim.g["test#python#runner"] = "pyunit" -- pytest

        -- Javascript
        vim.g["test#javascript#reactscripts#options"] = "--watchAll=false"
        vim.g["test#javascript#runner"] = "jest"
        vim.g["test#javascript#cypress#executable"] = "npx cypress run-ct"
      end,
    },
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
    { "nvim-neotest/neotest-vim-test" },
    { "nvim-neotest/neotest-python" },
    { "nvim-neotest/neotest-plenary" },
    { "nvim-neotest/neotest-go" },
    { "haydenmeade/neotest-jest" },
    { "rouge8/neotest-rust" },
  },
}

-- CodeCompanion
return {
  "olimorris/codecompanion.nvim",
  opts = {
    adapters = {
      copilot = function()
        return require("codecompanion.adapters").extend("copilot", {
          schema = {
            model = {
              default = "gpt-4o-2024-05-13",
            },
          },
        })
      end,
    },
    strategies = {
      chat = {
        adapter = "copilot",
      },
      inline = {
        adapter = "copilot",
      },
    },
  },
  cmd = { "CodeCompanion", "CodeCompanionChat", "CodeCompanionActions" },
  keys = {
    { "<leader>acc", "<cmd>CodeCompanionChat<cr>",    desc = "AI Chat" },
    { "<leader>aca", "<cmd>CodeCompanion<cr>",        desc = "Ask AI" },
    { "<leader>acr", "<cmd>CodeCompanionActions<cr>", desc = "AI Actions" },
  },
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
    "zbirenbaum/copilot.lua",
  },
}

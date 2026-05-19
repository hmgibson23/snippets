return {
  "linux-cultist/venv-selector.nvim",
  branch = "regexp",
  cmd = "VenvSelect",
  keys = {
    { "<leader>lv", "<cmd>VenvSelect<cr>", desc = "Ven[v] Select" },
  },
  opts = {
    name = "venv",
    auto_refresh = true,
    options = {
      picker = "snacks",
    },
  },
  dependencies = {
    "neovim/nvim-lspconfig",
    "folke/snacks.nvim",
    { "mfussenegger/nvim-dap-python", lazy = true },
  },
}

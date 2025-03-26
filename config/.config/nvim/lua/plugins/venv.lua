return {
  "linux-cultist/venv-selector.nvim",
  branch = "regexp",
  opts = {
    name = "venv",
    auto_refresh = true,
  },
  dependencies = { "neovim/nvim-lspconfig", "nvim-telescope/telescope.nvim", "mfussenegger/nvim-dap-python" },
  config = function()
    local whichkey = require("which-key")
    whichkey.add({
      { "<leader>lv", "<cmd>VenvSelect<cr>", desc = "Ven[v] Select" },
    })
  end,
}

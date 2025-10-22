return {
  "GeorgesAlkhouri/nvim-aider",
  cmd = "Aider",
  keys = {
    { "<leader>ai/", "<cmd>Aider toggle<cr>",       desc = "Toggle Aider" },
    { "<leader>ais", "<cmd>Aider send<cr>",         desc = "Send to Aider",                  mode = { "n", "v" } },
    { "<leader>aic", "<cmd>Aider command<cr>",      desc = "Aider Commands" },
    { "<leader>aib", "<cmd>Aider buffer<cr>",       desc = "Send Buffer" },
    { "<leader>ai+", "<cmd>Aider add<cr>",          desc = "Add File" },
    { "<leader>ai-", "<cmd>Aider drop<cr>",         desc = "Drop File" },
    { "<leader>air", "<cmd>Aider add readonly<cr>", desc = "Add Read-Only" },
    { "<leader>aiR", "<cmd>Aider reset<cr>",        desc = "Reset Session" },
    { "<leader>ai+", "<cmd>AiderTreeAddFile<cr>",   desc = "Add File from Tree to Aider",    ft = "NvimTree" },
    { "<leader>ai-", "<cmd>AiderTreeDropFile<cr>",  desc = "Drop File from Tree from Aider", ft = "NvimTree" },
  },
  dependencies = {
    "folke/snacks.nvim",
    "catppuccin/nvim",
    "nvim-tree/nvim-tree.lua",
    {
      "nvim-neo-tree/neo-tree.nvim",
      opts = function(_, opts)
        require("nvim_aider.neo_tree").setup(opts)
      end,
    },
  },
  config = true,
}

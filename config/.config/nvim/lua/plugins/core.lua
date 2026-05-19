return {
  { "nvim-neotest/nvim-nio", lazy = true },
  { "JafarDakhan/vim-gml", ft = "gml" },
  { "ahayworth/ink-syntax-vim", ft = "ink" },
  {
    "RostislavArts/naysayer.nvim",
    priority = 1000,
    lazy = false,
    config = function()
      vim.cmd.colorscheme("naysayer")
    end,
  },
  {
    "mbbill/undotree",
    cmd = "UndotreeToggle",
    keys = {
      { "<leader>u", "<cmd>UndotreeToggle<cr>", desc = "Undo tree" },
    },
  },
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    ---@type Flash.Config
    opts = {},
    -- stylua: ignore
    keys = {
      { "s",     mode = { "n", "x", "o" }, function() require("flash").jump() end,              desc = "Flash" },
      { "S",     mode = { "n", "x", "o" }, function() require("flash").treesitter() end,        desc = "Flash Treesitter" },
      { "r",     mode = "o",               function() require("flash").remote() end,            desc = "Remote Flash" },
      { "R",     mode = { "o", "x" },      function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
      { "<c-s>", mode = { "c" },           function() require("flash").toggle() end,            desc = "Toggle Flash Search" },
    },
  },
  { "nvim-lua/plenary.nvim", lazy = true },
  { "catppuccin/nvim", name = "catppuccin", priority = 1000, lazy = true },
  {
    "sindrets/diffview.nvim",
    cmd = { "DiffviewOpen", "DiffviewFileHistory" },
  },
  {
    "teal-language/vim-teal",
    ft = "teal",
  },
  {
    "filipdutescu/renamer.nvim",
    cmd = "Renamer",
  },
  { "kevinhwang91/nvim-bqf", ft = "qf" },
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre", "BufNewFile" },
    opts = {},
  },
  { "nvim-tree/nvim-web-devicons", lazy = true, opts = { default = true } },
  {
    "folke/neoconf.nvim",
    cmd = "Neoconf",
  },
  {
    "GCBallesteros/jupytext.nvim",
    ft = { "ipynb", "quarto", "qmd", "markdown" },
    config = true,
  },
  {
    "aznhe21/actions-preview.nvim",
    event = "LspAttach",
    dependencies = { "folke/snacks.nvim" },
    config = function()
      require("actions-preview").setup({
        backend = { "snacks" },
        diff = {
          algorithm = "patience",
          ignore_whitespace = true,
        },
      })
    end,
  },
}

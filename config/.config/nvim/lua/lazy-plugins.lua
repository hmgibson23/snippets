require("lazy").setup({
  "nvim-neotest/nvim-nio",
  "JafarDakhan/vim-gml",
  { "ahayworth/ink-syntax-vim", ft = "ink" },
  { "pablopunk/pi.nvim" },
  {
    "RostislavArts/naysayer.nvim",
    priority = 1000,
    lazy = false,
    config = function()
      vim.cmd.colorscheme("naysayer")
    end,
  },
  {
    "A7Lavinraj/fyler.nvim",
    dependencies = { "nvim-mini/mini.icons" },
    branch = "stable",
    opts = {},
  },
  {
    "mbbill/undotree",
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
  { "catppuccin/nvim", name = "catppuccin", priority = 1000 },
  {
    "sindrets/diffview.nvim",
    cmd = { "DiffviewOpen", "DiffviewFileHistory" },
  },
  {
    "teal-language/vim-teal",
    ft = "teal",
  },
  "filipdutescu/renamer.nvim",
  { "kevinhwang91/nvim-bqf", ft = "qf" },
  "lewis6991/gitsigns.nvim",
  "LukasPietzschmann/boo.nvim",
  { "nvim-tree/nvim-web-devicons", lazy = true, opts = { default = true } },
  "hedyhli/outline.nvim",
  "roobert/hoversplit.nvim",
  "folke/neoconf.nvim",
  {
    "GCBallesteros/jupytext.nvim",
    config = true,
  },
  -- "ggandor/leap.nvim",
  require("plugins/which-key"),
  require("plugins/oil"),
  require("plugins/dap"),
  require("plugins/done"),
  require("plugins/mini"),
  require("plugins/nightfox"),
  require("plugins/treesitter"),
  require("plugins/snacks"),
  require("plugins/venv"),
  require("plugins/otter"),
  require("plugins/lspconfig"),
  require("plugins/cmake-tools"),
  require("plugins/lspsaga"),
  require("plugins/none-ls"),
  require("plugins/cmp"),
  require("plugins/codeium"),
  require("plugins/hslens"),
  require("plugins/supermaven"),
  require("plugins/toggleterm"),
  require("plugins/lualine"),
  require("plugins/neogit"),
  require("plugins/iron"),
  require("plugins/overseer"),
  require("plugins/quarto"),
  require("plugins/neotest"),
  require("plugins/harpoon"),
  require("plugins/copilot"),
  -- require("plugins/aider"),
  require("plugins/codecompanion"),
  require("plugins/opencode"),
  require("plugins/pi"),
  -- LSP enhancement plugins
  {
    "kosayoda/nvim-lightbulb",
    event = "LspAttach",
    opts = {
      autocmd = { enabled = true },
      sign = { enabled = false },
      virtual_text = { enabled = true, text = "💡" },
    },
  },
  {
    "aznhe21/actions-preview.nvim",
    event = "LspAttach",
    config = function()
      require("actions-preview").setup({
        backend = { "telescope" },
        diff = {
          algorithm = "patience",
          ignore_whitespace = true,
        },
        telescope = vim.tbl_extend(
          "force",
          require("telescope.themes").get_dropdown(),
          { make_value = nil, make_make_display = nil }
        ),
      })
    end,
  },
}, {
  ui = {
    icons = vim.g.have_nerd_font and {} or {
      cmd = "⌘",
      config = "🛠",
      event = "📅",
      ft = "📂",
      init = "⚙",
      keys = "🗝",
      plugin = "🔌",
      runtime = "💻",
      require = "🌙",
      source = "📄",
      start = "🚀",
      task = "📌",
      lazy = "💤 ",
    },
  },
})

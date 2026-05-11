return {
  "quarto-dev/quarto-nvim",
  ft = { "quarto", "qmd", "markdown" },
  cmd = { "QuartoActivate", "QuartoPreview" },
  keys = {
    { "<leader>q", group = "Quarto" },
    {
      "<leader>qc",
      function()
        require("quarto.runner").run_cell()
      end,
      desc = "Run Cell",
    },
    { "<leader>qs", "<cmd>QuartoActivate<cr>", desc = "Activate Quarto" },
    {
      "<leader>qa",
      function()
        require("quarto.runner").run_above()
      end,
      desc = "Run Cell and Above",
    },
    {
      "<leader>qA",
      function()
        require("quarto.runner").run_all()
      end,
      desc = "Run All Cells",
    },
    { "<leader>qp", "<cmd>QuartoPreview<cr>", desc = "Preview Quarto" },
  },
  opts = {
    lspFeatures = {
      languages = { "r", "python", "rust" },
      chunks = "all",
      diagnostics = {
        enabled = true,
        triggers = { "BufWritePost" },
      },
      completion = {
        enabled = true,
      },
    },
    keymap = {
      hover = "H",
      definition = "gd",
      rename = "<leader>rn",
      references = "gr",
      format = "<leader>gf",
    },
    codeRunner = {
      enabled = true,
      default_method = "iron",
      ft_runners = { python = "iron" },
    },
    plotPreview = {
      enabled = true,
      method = "browser",
    },
  },
}

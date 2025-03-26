return {
  "quarto-dev/quarto-nvim",
  config = function()
    local quarto = require("quarto")

    quarto.setup({
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
    })
    local whichkey = require("which-key")
    whichkey.add({
      { "<leader>q",  group = "Quarto" },
      { "<leader>qc", "<cmd>lua require('quarto.runner').run_cell()<cr>",  desc = "Run Cell" },
      { "<leader>qs", "<cmd>QuartoActivate<cr>",                           desc = "Activate Quarto" },
      { "<leader>qa", "<cmd>lua require('quarto.runner').run_above()<cr>", desc = "Run Cell and Above" },
      { "<leader>qA", "<cmd>lua require('quarto.runner').run_all()<cr>",   desc = "Run All Cells" },
      { "<leader>qp", "<cmd>QuartoPreview<cr>",                            desc = "Preview Quarto" },
    })
  end,
}

return {
  {
    -- Better folding with Treesitter and indentation support
    "kevinhwang91/nvim-ufo",
    lazy = true,
    keys = { "zc", "zo", "zR", "zm" },
    dependencies = { "kevinhwang91/promise-async" },
    opts = {
      provider_selector = function(_, _)
        return { "treesitter", "indent" }
      end,
    },
    config = function(_, opts)
      require("ufo").setup(opts)
      vim.keymap.set("n", "zR", require("ufo").openAllFolds, { desc = "Open all folds" })
      vim.keymap.set("n", "zM", require("ufo").closeAllFolds, { desc = "Close all folds" })
    end,
    enabled = false,
  },
}

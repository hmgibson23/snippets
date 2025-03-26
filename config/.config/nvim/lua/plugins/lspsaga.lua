return {
  {
    "nvimdev/lspsaga.nvim",

    {
      "folke/trouble.nvim",
      cmd = { "TroubleToggle", "Trouble" },
      config = function()
        require("trouble").setup({
          use_diagnostic_signs = true,
        })
      end,
    },
    config = function()
      require("lspsaga").setup({})
    end,
    dependencies = {
      "nvim-treesitter/nvim-treesitter", -- optional
      "nvim-tree/nvim-web-devicons",  -- optional
    },
  },
}

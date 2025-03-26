return {
  "neovim/nvim-lspconfig",
  config = function()
    local servers = require("plugins.lsp.servers")
    local server_options = require("plugins.lsp.options")
    require("plugins.lsp.installer").setup(servers, server_options)
    require("plugins.lsp.null-ls").setup(server_options)

    local lspconfig = require("lspconfig")
    lspconfig.sourcekit.setup({
      capabilities = {
        workspace = {
          didChangeWatchedFiles = {
            dynamicRegistration = true,
          },
        },
      },
    })
  end,
  dependencies = {
    "williamboman/mason.nvim",
    {
      "mrjones2014/legendary.nvim",
      version = "v2.13.9",
      priority = 10000,
      lazy = false,
    },
    {
      "chrisgrieser/nvim-dr-lsp",
      event = "LspAttach",
      opts = {},
    },
    "williamboman/mason-lspconfig.nvim",
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    { "jayp0521/mason-null-ls.nvim" },
    "ray-x/lsp_signature.nvim",
    "VidocqH/lsp-lens.nvim",
    "folke/lua-dev.nvim",
    "RRethy/vim-illuminate",
    "zeioth/garbage-day.nvim",

    {
      "dnlhc/glance.nvim",
      cmd = "Glance",
    },
    {
      "Wansmer/symbol-usage.nvim",
      event = "LspAttach",
      config = function()
        require("symbol-usage").setup({})
      end,
    },
    "nvimtools/none-ls.nvim",
    "b0o/schemastore.nvim",
    { "jose-elias-alvarez/typescript.nvim" },
    "alpha2phi/nvim-navic",
    config = function()
      require("nvim-navic").setup({})
    end,
    {
      "simrat39/inlay-hints.nvim",
      config = function()
        require("inlay-hints").setup()
      end,
    },
  },
}

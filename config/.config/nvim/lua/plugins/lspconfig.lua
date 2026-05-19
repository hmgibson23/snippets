-- LSP Plugins
return {
  {
    "folke/lazydev.nvim",
    ft = "lua",
    opts = {
      library = {
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  },
  {
    "Hoffs/omnisharp-extended-lsp.nvim",
    ft = "cs",
  },
  {
    "smjonas/inc-rename.nvim",
    cmd = "IncRename",
    config = function()
      require("inc_rename").setup()
    end,
  },
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "folke/which-key.nvim",
      {
        "williamboman/mason.nvim",
        opts = {
          registries = {
            "github:mason-org/mason-registry",
            "github:Crashdummyy/mason-registry",
          },
        },
      },
      "williamboman/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",

      { "j-hui/fidget.nvim",       opts = {} },
      {
        "amrbashir/nvim-docs-view",
        lazy = true,
        cmd = "DocsViewToggle",
        opts = {
          position = "right",
          width = 60,
        },
      },
      "hrsh7th/cmp-nvim-lsp",

      {
        "Wansmer/symbol-usage.nvim",
        event = "LspAttach",
        config = function()
          require("symbol-usage").setup({})
        end,
      },
      "b0o/schemastore.nvim",
    },
    config = function()
      local highlight_augroup = vim.api.nvim_create_augroup("user-lsp-highlight", { clear = true })
      local detach_augroup = vim.api.nvim_create_augroup("user-lsp-detach", { clear = true })

      -- The "on attach" style LspAttach autocmd
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("user-lsp-attach", { clear = true }),
        callback = function(event)
          local keymaps = require("plugins.lsp.keymaps")
          local whichkey = require("which-key")
          whichkey.add(keymaps.whichkey)
          keymaps.setup(event.buf)

          local client = vim.lsp.get_client_by_id(event.data.client_id)
          if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, event.buf) then
            vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
              buffer = event.buf,
              group = highlight_augroup,
              callback = vim.lsp.buf.document_highlight,
            })
            vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
              buffer = event.buf,
              group = highlight_augroup,
              callback = vim.lsp.buf.clear_references,
            })
            vim.api.nvim_create_autocmd("LspDetach", {
              buffer = event.buf,
              group = detach_augroup,
              callback = function(event2)
                vim.lsp.buf.clear_references()
                vim.api.nvim_clear_autocmds({ group = highlight_augroup, buffer = event2.buf })
              end,
            })
          end

          if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf) then
            vim.keymap.set("n", "<leader>th", function()
              local opts = { bufnr = event.buf }
              if vim.lsp.inlay_hint.is_enabled(opts) then
                vim.lsp.inlay_hint.disable(opts)
              else
                vim.lsp.inlay_hint.enable(opts)
              end
            end, { buffer = event.buf, desc = "LSP: [T]oggle Inlay [H]ints" })
          end
        end,
      })

      -- Diagnostic signs
      if vim.g.have_nerd_font then
        local signs = { ERROR = "", WARN = "", INFO = "", HINT = "" }
        local diagnostic_signs = {}
        for type, icon in pairs(signs) do
          diagnostic_signs[vim.diagnostic.severity[type]] = icon
        end
        vim.diagnostic.config({ signs = { text = diagnostic_signs } })
      end

      -- Build up capabilities once
      local cmp_nvim_lsp = require("cmp_nvim_lsp")
      local base_capabilities = vim.lsp.protocol.make_client_capabilities()
      local capabilities = vim.tbl_deep_extend("force", base_capabilities, cmp_nvim_lsp.default_capabilities())

      -- Load your custom server and tool inventories.
      local servers = require("plugins.lsp.servers")
      local tools = require("plugins.lsp.tools")

      -- Automatically ensure LSP servers and formatter/linter tools are installed.
      local ensure_installed = vim.tbl_keys(servers or {})
      vim.list_extend(ensure_installed, tools.mason_packages())
      require("mason-tool-installer").setup({ ensure_installed = ensure_installed })

      local function disable_client_formatting(client)
        client.server_capabilities.documentFormattingProvider = false
        client.server_capabilities.documentRangeFormattingProvider = false
      end

      -- For each server define/customize via vim.lsp.config
      for server_name, server_opts in pairs(servers) do
        server_opts = vim.deepcopy(server_opts)

        -- merge in things like capabilities, etc
        server_opts.capabilities =
            vim.tbl_deep_extend("force", {}, capabilities, server_opts.capabilities or {})

        if server_opts.disable_formatting then
          local on_attach = server_opts.on_attach
          server_opts.on_attach = function(client, bufnr)
            disable_client_formatting(client)
            if on_attach then
              on_attach(client, bufnr)
            end
          end
          server_opts.disable_formatting = nil
        end

        -- Define the server config
        vim.lsp.config(server_name, server_opts)
      end

      -- Then enable the servers
      for server_name, _ in pairs(servers) do
        vim.lsp.enable(server_name)
      end

      -- Custom server configs that need extra setup

      -- clangd
      vim.lsp.config("clangd", {
        capabilities = capabilities,
        cmd = {
          "clangd",
          "--offset-encoding=utf-16",
          "--background-index",
          "--clang-tidy",
          "--header-insertion=iwyu",
          "--completion-style=detailed",
          "--function-arg-placeholders=true",
        },
        init_options = {
          fallbackFlags = { "-std=c++23" },
          compilationDatabaseChangedTimeout = 2000,
        },
        single_file_support = true,
        flags = {
          debounce_text_changes = 150,
        },
        root_dir = require("lspconfig.util").root_pattern(
          ".clangd",
          ".clang-tidy",
          ".clang-format",
          "compile_commands.json",
          "compile_flags.txt",
          "configure.ac",
          ".git"
        ),
      })
      vim.lsp.enable("clangd")

      -- C# is handled by seblyng/roslyn.nvim in lua/plugins/csharp.lua.
      -- Keep OmniSharp installed for manual fallback, but do not start both LSPs.
    end,
  },
}

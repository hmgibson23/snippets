local M = {}

function M.setup(servers, server_options)
  local lspconfig = require("lspconfig")
  local icons = require("plugins.icons")

  require("mason").setup({
    ui = {
      icons = {
        package_installed = icons.lsp.server_installed,
        package_pending = icons.lsp.server_pending,
        package_uninstalled = icons.lsp.server_uninstalled,
      },
    },
  })

  require("mason-null-ls").setup({
    automatic_setup = true,
  })

  require("mason-tool-installer").setup({
    ensure_installed = { "codelldb", "stylua", "shfmt", "shellcheck", "prettierd", "terraform-ls", "prettierd" },
    auto_update = true,
    run_on_start = true,
  })

  require("mason-lspconfig").setup({
    ensure_installed = vim.tbl_keys(servers),
    automatic_installation = false,
  })

  -- Package installation folder
  local install_root_dir = vim.fn.stdpath("data") .. "/mason"

  require("mason-lspconfig").setup_handlers({
    function(server_name)
      local opts = vim.tbl_deep_extend("force", server_options, servers[server_name] or {})
      lspconfig[server_name].setup(opts)
    end,
    ["jdtls"] = function() end,
    ["awk_ls"] = function() end,
    ["lua_ls"] = function()
      local opts = vim.tbl_deep_extend("force", server_options, servers["lua_ls"] or {})
      require("neodev").setup({ lspconfig = opts })
      lspconfig.lua_ls.setup({})
    end,
    ["clangd"] = function()
      local opts = vim.tbl_deep_extend("force", server_options, servers["clangd"] or {})
      print("clanging")
      require("clangd_extensions.inlay_hints").setup_autocmd()
      require("clangd_extensions.inlay_hints").set_inlay_hints()
      print(opts)
    end,
    ["terraformls"] = function()
      vim.api.nvim_create_autocmd({ "BufWritePre" }, {
        pattern = { "*.tf", "*.tfvars" },
        callback = function()
          vim.lsp.buf.format()
        end,
      })
    end,
    ["ts_ls"] = function()
      local opts = vim.tbl_deep_extend("force", server_options, servers["ts_ls"] or {})
      require("typescript").setup({
        disable_commands = false,
        debug = false,
        server = opts,
      })
    end,
  })
end

return M

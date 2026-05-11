return {
  "nvimtools/none-ls.nvim",
  event = { "BufReadPre", "BufNewFile" },
  opts = function(_, opts)
    -- Placeholder for further customization if needed
  end,
  config = function()
    local nls = require("null-ls")
    local server_options = require("plugins.lsp.options")
    local b = nls.builtins

    -- 🔹 Define sources for formatting, diagnostics, code actions, and hover
    local sources = {
      -- 🔹 Formatting
      b.formatting.prettierd,
      b.formatting.shfmt,
      b.formatting.black.with({ extra_args = { "--fast" } }),
      b.formatting.isort,
      b.formatting.stylua.with({
        filetypes = { "lua", "teal", "tl", "script" },
      }),
      b.formatting.google_java_format,
      b.formatting.clang_format,
      -- b.formatting.swiftformat, -- Uncomment if needed

      -- 🔹 Diagnostics
      b.diagnostics.checkmake,
      b.diagnostics.hadolint,
      b.diagnostics.markdownlint_cli2,
      b.diagnostics.pylint,
      b.diagnostics.write_good,
      b.diagnostics.cmake_lint,
      b.diagnostics.cppcheck,
      -- b.diagnostics.clazy,
      b.diagnostics.zsh,
      b.diagnostics.swiftlint,
      -- b.diagnostics.teal,

      -- 🔹 Code Actions
      b.code_actions.gitsigns.with({
        disabled_filetypes = { "NeogitCommitMessage" },
      }),
      b.code_actions.gitrebase,
      b.code_actions.refactoring,
      b.code_actions.proselint,

      -- 🔹 Terraform
      b.diagnostics.terraform_validate,
      b.diagnostics.tfsec,
      b.formatting.terraform_fmt,

      -- 🔹 Hover
      b.hover.dictionary,
    }

    -- 🔹 Auto-group for LSP Formatting
    local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

    -- 🔹 Setup null-ls with enhanced options
    nls.setup({
      sources = sources,
      debug = true,
      on_attach = function(client, bufnr)
        if client:supports_method("textDocument/formatting", bufnr) then
          vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
          vim.api.nvim_create_autocmd("BufWritePre", {
            group = augroup,
            buffer = bufnr,
            callback = function()
              -- Neovim 0.8+ compatibility
              vim.lsp.buf.format({ bufnr = bufnr, async = false })
            end,
          })
        end
      end,

      root_dir = require("null-ls.utils").root_pattern(".git"),
    })
  end,
}

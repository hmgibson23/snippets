local function executable(command)
  return vim.fn.executable(command) == 1
end

local function add_if_available(sources, source, command)
  if source and (not command or executable(command)) then
    table.insert(sources, source)
  end
end

return {
  "nvimtools/none-ls.nvim",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    local nls = require("null-ls")
    local b = nls.builtins
    local sources = {}

    -- Formatting: only register tools that are actually executable to avoid
    -- repeated none-ls generator failures and noisy notifications/logs.
    add_if_available(sources, b.formatting.prettierd, "prettierd")
    add_if_available(sources, b.formatting.shfmt, "shfmt")
    add_if_available(sources, b.formatting.black.with({ extra_args = { "--fast" } }), "black")
    add_if_available(sources, b.formatting.isort, "isort")
    add_if_available(sources, b.formatting.stylua.with({
      filetypes = { "lua", "teal", "tl", "script" },
    }), "stylua")
    add_if_available(sources, b.formatting.google_java_format, "google-java-format")
    add_if_available(sources, b.formatting.clang_format, "clang-format")
    add_if_available(sources, b.formatting.csharpier, "csharpier")
    add_if_available(sources, b.formatting.terraform_fmt, "terraform")

    -- Diagnostics
    add_if_available(sources, b.diagnostics.checkmake, "checkmake")
    add_if_available(sources, b.diagnostics.hadolint, "hadolint")
    add_if_available(sources, b.diagnostics.markdownlint_cli2, "markdownlint-cli2")
    add_if_available(sources, b.diagnostics.pylint, "pylint")
    add_if_available(sources, b.diagnostics.write_good, "write-good")
    add_if_available(sources, b.diagnostics.cmake_lint, "cmake-lint")
    add_if_available(sources, b.diagnostics.cppcheck, "cppcheck")
    add_if_available(sources, b.diagnostics.zsh, "zsh")
    add_if_available(sources, b.diagnostics.swiftlint, "swiftlint")
    add_if_available(sources, b.diagnostics.terraform_validate, "terraform")
    add_if_available(sources, b.diagnostics.tfsec, "tfsec")

    -- Code actions / hovers
    add_if_available(sources, b.code_actions.gitsigns)
    add_if_available(sources, b.code_actions.gitrebase, "git")
    add_if_available(sources, b.code_actions.refactoring)
    add_if_available(sources, b.code_actions.proselint, "proselint")
    add_if_available(sources, b.hover.dictionary)

    local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

    nls.setup({
      sources = sources,
      debug = false,
      on_attach = function(client, bufnr)
        if client:supports_method("textDocument/formatting", bufnr) then
          vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
          vim.api.nvim_create_autocmd("BufWritePre", {
            group = augroup,
            buffer = bufnr,
            callback = function()
              vim.lsp.buf.format({
                bufnr = bufnr,
                async = false,
                filter = function(format_client)
                  return format_client.name == "null-ls"
                end,
              })
            end,
          })
        end
      end,
      root_dir = require("null-ls.utils").root_pattern(".git"),
    })
  end,
}

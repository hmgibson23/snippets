local tools = require("plugins.lsp.tools")

local function executable(command)
  return vim.fn.executable(command) == 1
end

local function add_if_available(sources, source, command)
  if source and (not command or executable(command)) then
    table.insert(sources, source)
  end
end

local function resolve_builtin(group, spec)
  local source = group[spec.builtin]
  if source and spec.opts then
    source = source.with(spec.opts)
  end
  return source
end

local function add_tool_group(sources, builtins, specs)
  for _, spec in ipairs(specs) do
    add_if_available(sources, resolve_builtin(builtins, spec), spec.command)
  end
end

return {
  "nvimtools/none-ls.nvim",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    local nls = require("null-ls")
    local b = nls.builtins
    local sources = {}

    -- Only register tools that are actually executable to avoid repeated
    -- none-ls generator failures and noisy notifications/logs.
    add_tool_group(sources, b.formatting, tools.formatters)
    add_tool_group(sources, b.diagnostics, tools.linters)
    add_tool_group(sources, b.code_actions, tools.code_actions)
    add_tool_group(sources, b.hover, tools.hovers)

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

local tools = require("plugins.lsp.tools")

local function executable(command)
  return vim.fn.executable(command) == 1
end

local function resolve_builtin(group, spec)
  local source = group[spec.builtin]
  if source and spec.opts then
    source = source.with(spec.opts)
  end
  return source
end

return {
  "nvimtools/none-ls.nvim",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    local nls = require("null-ls")
    local b = nls.builtins
    local mason = require("plugins.lsp.mason")
    local registered = {}

    local function register_spec(group, spec)
      local source = resolve_builtin(group, spec)
      if not source or (spec.command and not executable(spec.command)) then
        return
      end

      if registered[source.name] or nls.is_registered(source.name) then
        registered[source.name] = true
        return
      end

      nls.register(source)
      registered[source.name] = true
    end

    local function register_group(group, specs)
      for _, spec in ipairs(specs) do
        register_spec(group, spec)
      end
    end

    local function group_for_spec(spec)
      for _, candidate in ipairs(tools.formatters) do
        if candidate == spec then
          return b.formatting
        end
      end
      for _, candidate in ipairs(tools.linters) do
        if candidate == spec then
          return b.diagnostics
        end
      end
      return b.code_actions
    end

    -- Register already available tools. Missing Mason tools are registered after
    -- their on-demand install completes for the current buffer's filetype.
    register_group(b.formatting, tools.formatters)
    register_group(b.diagnostics, tools.linters)
    register_group(b.code_actions, tools.code_actions)
    register_group(b.hover, tools.hovers)

    local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

    nls.setup({
      sources = {},
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

    vim.api.nvim_create_autocmd("FileType", {
      group = vim.api.nvim_create_augroup("user-none-ls-on-demand", { clear = true }),
      callback = function(event)
        local filetype = vim.bo[event.buf].filetype
        local specs = tools.for_filetype(filetype)
        mason.ensure_specs(specs, "formatter/linter tools for *" .. filetype .. "*", function(ok)
          if not ok then
            return
          end

          for _, spec in ipairs(specs) do
            register_spec(group_for_spec(spec), spec)
          end
        end)
      end,
    })
  end,
}

local select = require("workflows.select")

local M = {}

local function action(title, fn)
  return { label = title, action = fn }
end

function M.format()
  vim.lsp.buf.format({ async = true })
end

function M.organize_imports()
  vim.lsp.buf.code_action({
    apply = true,
    context = { only = { "source.organizeImports" }, diagnostics = {} },
  })
end

function M.fix_all()
  vim.lsp.buf.code_action({
    apply = true,
    context = { only = { "source.fixAll" }, diagnostics = {} },
  })
end

function M.actions()
  return {
    action("Code action", vim.lsp.buf.code_action),
    action("Rename symbol", vim.lsp.buf.rename),
    action("Format buffer", M.format),
    action("Organize imports", M.organize_imports),
    action("Fix all", M.fix_all),
    action("Diagnostics: line", vim.diagnostic.open_float),
    action("Diagnostics: buffer", function()
      if package.loaded["snacks"] then
        Snacks.picker.diagnostics_buffer()
      else
        vim.diagnostic.setloclist()
      end
    end),
    action("Symbols: document", function()
      if package.loaded["snacks"] then
        Snacks.picker.lsp_symbols()
      else
        vim.lsp.buf.document_symbol()
      end
    end),
    action("Symbols: workspace", function()
      if package.loaded["snacks"] then
        Snacks.picker.lsp_workspace_symbols()
      else
        vim.lsp.buf.workspace_symbol("")
      end
    end),
  }
end

function M.palette()
  select.run("Code actions", M.actions())
end

function M.setup()
  vim.api.nvim_create_user_command("CodePalette", M.palette, { desc = "Code action palette" })
  vim.api.nvim_create_user_command("LspOrganizeImports", M.organize_imports, { desc = "Organize imports with LSP" })
  vim.api.nvim_create_user_command("LspFixAll", M.fix_all, { desc = "Apply source.fixAll code action" })

  vim.keymap.set({ "n", "v" }, "<leader>ca", M.palette, { desc = "Code action palette" })
  vim.keymap.set("n", "<leader>cF", M.format, { desc = "Format buffer" })
  vim.keymap.set("n", "<leader>co", M.organize_imports, { desc = "Organize imports" })
  vim.keymap.set("n", "<leader>cx", M.fix_all, { desc = "Fix all" })
end

return M

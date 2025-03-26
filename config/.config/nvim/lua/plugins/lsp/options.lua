
local M = {}

M.on_attach = function(client, bufnr)
  local caps = client.server_capabilities

  vim.lsp.codelens.refresh()

  -- Enable omnifunc if the server provides completion
  if caps.completionProvider then
    vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"
  end

  -- Use LSP for formatting (if provided)
  if caps.documentFormattingProvider then
    vim.bo[bufnr].formatexpr = "v:lua.vim.lsp.formatexpr()"
  end

  -- Configure key mappings, highlighting, and formatting
  require("plugins.lsp.keymaps").setup(client, bufnr)
  require("plugins.lsp.highlighter").setup(client, bufnr)
  require("plugins.lsp.null-ls.formatters").setup(client, bufnr)

  -- Set tagfunc for definition jumps
  if caps.definitionProvider then
    vim.bo[bufnr].tagfunc = "v:lua.vim.lsp.tagfunc"
  end

  -- Special setup for SQL language server
  if client.name == "sqls" then
    require("sqls").on_attach(client, bufnr)
  end

  -- Special setup for jdtls
  if client.name == "jdtls" then
    print(client.name)
    vim.lsp.codelens.refresh()
    require("jdtls").setup_dap({ hotcodereplace = "auto" })
    require("jdtls.dap").setup_dap_main_class_configs()
  end

  -- Attach nvim-navic for document symbols
  if caps.documentSymbolProvider then
    local navic = require("nvim-navic")
    navic.attach(client, bufnr)
  end

  -- Set a custom commentstring for terraform/hcl files
  if client.name ~= "terraformls" then
    vim.api.nvim_create_autocmd("FileType", {
      pattern = { "hcl", "terraform" },
      desc = "terraform/hcl commentstring configuration",
      command = "setlocal commentstring=#\\ %s",
    })
  end
end

-- Create default capabilities and enhance them for nvim-cmp
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.foldingRange = {
  dynamicRegistration = false,
  lineFoldingOnly = true,
}
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = {
    "documentation",
    "detail",
    "additionalTextEdits",
  },
}
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

M.capabilities = capabilities

M.flags = {
  debounce_text_changes = 150,
}

return M

-- local lsp_installer_servers = require "nvim-lsp-installer.servers"
-- local _ = require('mason.core.functional')
require('mason').setup()
local mason_lspconfig = require('mason-lspconfig')

local M = {}

function getTableKeys(tab)
  local keyset = {}
  for k,_ in pairs(tab) do
    keyset[#keyset + 1] = k
  end
  return keyset
end

function M.setup(servers, options)
  mason_lspconfig.setup({
      ensure_installed = getTableKeys(servers)
  })
  mason_lspconfig.setup_handlers({
      function (server_name)
        local opts = vim.tbl_deep_extend("force", options, servers[server_name] or {})
        local coq = require "coq"
        opts = coq.lsp_ensure_capabilities(opts)
        require('lspconfig')[server_name].setup(opts)
      end
    })
end

return M

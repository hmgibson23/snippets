-- ABOUTME: Feature capability registry for LSP enhancements
-- ABOUTME: Tracks what LSP capabilities each feature requires

local M = {}

-- Capability requirements for each feature
M.requirements = {
  diagnostics = {
    textDocument = {
      publishDiagnostics = {
        tagSupport = { valueSet = { 1, 2 } }, -- Unnecessary, Deprecated
        relatedInformation = true,
        codeDescriptionSupport = true,
      },
    },
  },

  actions = {
    textDocument = {
      codeAction = {
        dynamicRegistration = true,
        isPreferredSupport = true,
        disabledSupport = true,
        dataSupport = true,
        resolveSupport = {
          properties = { "edit", "command" },
        },
        codeActionLiteralSupport = {
          codeActionKind = {
            valueSet = {
              "",
              "quickfix",
              "refactor",
              "refactor.extract",
              "refactor.inline",
              "refactor.rewrite",
              "source",
              "source.organizeImports",
            },
          },
        },
      },
    },
  },
}

-- Build complete capability table by merging all requirements
function M.build_capabilities()
  local base = vim.lsp.protocol.make_client_capabilities()

  -- Merge in cmp capabilities if available
  local has_cmp, cmp_lsp = pcall(require, 'cmp_nvim_lsp')
  if has_cmp then
    base = cmp_lsp.default_capabilities(base)
  end

  -- Merge in our feature requirements
  for _, caps in pairs(M.requirements) do
    base = vim.tbl_deep_extend('force', base, caps)
  end

  return base
end

-- Check if a specific feature's capabilities are supported by client
function M.supports_feature(client, feature_name)
  local required_caps = M.requirements[feature_name]
  if not required_caps then
    return false
  end

  -- Walk the capability tree and check if all required capabilities exist
  local function check_capabilities(required, actual)
    for key, value in pairs(required) do
      if actual[key] == nil then
        return false
      end
      if type(value) == "table" then
        if not check_capabilities(value, actual[key]) then
          return false
        end
      end
    end
    return true
  end

  return check_capabilities(required_caps, client.server_capabilities)
end

return M

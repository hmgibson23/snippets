-- ABOUTME: Capability builder and manager for LSP enhancements
-- ABOUTME: Public interface for building and querying LSP capabilities

local registry = require('lsp.capabilities.registry')

local M = {}

-- Build complete LSP capabilities including all feature requirements
function M.build()
  return registry.build_capabilities()
end

-- Check if client supports a specific feature
function M.supports_feature(client, feature_name)
  return registry.supports_feature(client, feature_name)
end

-- Get raw capability requirements for a feature
function M.get_requirements(feature_name)
  return registry.requirements[feature_name]
end

return M

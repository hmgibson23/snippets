-- ABOUTME: Treesitter provider for scope detection and node navigation
-- ABOUTME: Handles finding appropriate Treesitter nodes for different scope levels

local Provider = require('lsp.providers')

local TreesitterProvider = Provider:new()

-- Scope type to Treesitter node type mappings (language-agnostic patterns)
local SCOPE_NODE_TYPES = {
  statement = {
    "expression_statement",
    "assignment_statement",
    "return_statement",
    "if_statement",
    "for_statement",
    "while_statement",
  },
  ["function"] = {
    "function_definition",
    "function_declaration",
    "method_definition",
    "arrow_function",
  },
  class = {
    "class_definition",
    "class_declaration",
  },
}

-- Get diagnostics for buffer
-- Treesitter doesn't provide diagnostics, only structural information
function TreesitterProvider:get_diagnostics(bufnr)
  return {}
end

-- Get code actions for buffer and range
-- Treesitter doesn't provide code actions
function TreesitterProvider:get_code_actions(bufnr, range)
  return {}
end

-- Get Treesitter node for scope
function TreesitterProvider:get_scope_node(bufnr, pos, scope_type)
  local ok, node = pcall(vim.treesitter.get_node, { bufnr = bufnr, pos = pos })
  if not ok or not node then
    return nil
  end

  -- For file scope, return root node
  if scope_type == "file" then
    while node:parent() do
      node = node:parent()
    end
    return node
  end

  -- For other scopes, walk up tree to find matching node type
  local target_types = SCOPE_NODE_TYPES[scope_type]
  if not target_types then
    return nil
  end

  while node do
    local node_type = node:type()
    for _, target_type in ipairs(target_types) do
      if node_type == target_type then
        return node
      end
    end
    node = node:parent()
  end

  return nil
end

-- Check if diagnostic can be suppressed
-- Treesitter doesn't handle suppression
function TreesitterProvider:can_suppress(diagnostic)
  return false
end

-- Suppress diagnostic at specified scope
-- Treesitter doesn't handle suppression
function TreesitterProvider:suppress(diagnostic, scope)
  return false
end

-- Check if provider is available for buffer
-- Available if treesitter parser exists for buffer
function TreesitterProvider:is_available(bufnr)
  local ok, parser = pcall(vim.treesitter.get_parser, bufnr)
  return ok and parser ~= nil
end

return TreesitterProvider

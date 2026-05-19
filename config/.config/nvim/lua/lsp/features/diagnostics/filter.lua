-- ABOUTME: Diagnostic display filtering for persisted suppressions
-- ABOUTME: Wraps vim.diagnostic handlers so suppressed diagnostics are not rendered

local M = {}

local wrapped = false
local original_handlers = {}
local handler_names = { "signs", "virtual_text", "virtual_lines", "underline" }

function M.wrap_handler(manager, handler)
  handler = handler or {}

  return {
    show = function(namespace, bufnr, diagnostics, opts)
      if type(handler.show) ~= "function" then
        return
      end

      local file = vim.api.nvim_buf_get_name(bufnr)
      local filtered = diagnostics or {}
      if file ~= "" and manager and type(manager.filter_diagnostics) == "function" then
        filtered = manager:filter_diagnostics(file, filtered)
      end

      handler.show(namespace, bufnr, filtered, opts)
    end,
    hide = function(namespace, bufnr)
      if type(handler.hide) == "function" then
        handler.hide(namespace, bufnr)
      end
    end,
  }
end

function M.install(manager)
  if wrapped then
    return
  end

  for _, name in ipairs(handler_names) do
    local handler = vim.diagnostic.handlers[name]
    if handler then
      original_handlers[name] = handler
      vim.diagnostic.handlers[name] = M.wrap_handler(manager, handler)
    end
  end

  wrapped = true
end

function M.refresh(bufnr)
  vim.schedule(function()
    if bufnr and vim.api.nvim_buf_is_valid(bufnr) then
      vim.diagnostic.show(nil, bufnr)
    else
      vim.diagnostic.show()
    end
  end)
end

function M.reset_for_tests()
  for name, handler in pairs(original_handlers) do
    vim.diagnostic.handlers[name] = handler
  end
  original_handlers = {}
  wrapped = false
end

return M

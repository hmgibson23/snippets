local M = {}

local function has_module(name)
  local ok = pcall(require, name)
  return ok
end

local function dap()
  return require("dap")
end

local function dapui()
  return require("dapui")
end

function M.start_smart()
  if vim.bo.filetype == "python" then
    local ok, python_dap = pcall(require, "dap.python")
    if ok then
      return python_dap.start()
    end
  end
  return dap().continue()
end

function M.continue_or_start()
  local d = dap()
  if vim.bo.filetype == "python" then
    local ok, python_dap = pcall(require, "dap.python")
    if ok then
      return python_dap.continue()
    end
  end
  return d.continue()
end

function M.terminate()
  local d = dap()
  d.terminate()
  dapui().close()
end

function M.toggle_breakpoint()
  dap().toggle_breakpoint()
end

function M.conditional_breakpoint()
  dap().set_breakpoint(vim.fn.input("Breakpoint condition: "))
end

function M.logpoint()
  dap().set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
end

function M.eval_input()
  dapui().eval(vim.fn.input("Expression: "))
end

function M.hover()
  require("dap.ui.widgets").hover()
end

function M.scopes()
  local widgets = require("dap.ui.widgets")
  widgets.centered_float(widgets.scopes)
end

function M.frames()
  local widgets = require("dap.ui.widgets")
  widgets.centered_float(widgets.frames)
end

function M.repl()
  dap().repl.toggle({}, "vsplit")
end

function M.run_last()
  dap().run_last()
end

function M.debug_nearest_test()
  local ok, neotest = pcall(require, "neotest")
  if ok then
    return neotest.run.run({ strategy = "dap" })
  end
  vim.notify("neotest is not available", vim.log.levels.WARN)
end

function M.debug_file_tests()
  local ok, neotest = pcall(require, "neotest")
  if ok then
    return neotest.run.run({ vim.fn.expand("%"), strategy = "dap" })
  end
  vim.notify("neotest is not available", vim.log.levels.WARN)
end

function M.info()
  local d = dap()
  local ft = vim.bo.filetype
  local configs = d.configurations[ft] or {}
  local adapters = {}
  for name, _ in pairs(d.adapters or {}) do
    table.insert(adapters, name)
  end
  table.sort(adapters)

  local lines = {
    "DAP status",
    "==========",
    "filetype: " .. (ft ~= "" and ft or "<none>"),
    "session: " .. (d.session() and "active" or "none"),
    "configs for filetype: " .. tostring(#configs),
    "adapters: " .. (#adapters > 0 and table.concat(adapters, ", ") or "none"),
    "",
    "tooling:",
    "  debugpy: " .. (vim.fn.executable("debugpy-adapter") == 1 and "ok" or "install via :DapInstall python / Mason"),
    "  node: " .. (vim.fn.executable("node") == 1 and "ok" or "missing"),
    "  go/delve: " .. (vim.fn.executable("dlv") == 1 and "ok" or "install via :DapInstall delve / Mason"),
    "  cpptools: " .. (d.adapters.cppdbg and "configured" or "missing"),
    "  dap-ui: " .. (has_module("dapui") and "ok" or "missing"),
  }

  vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO, { title = "Debug info" })
end

function M.palette()
  local items = {
    { label = "Start / continue", action = M.continue_or_start },
    { label = "Start smart", action = M.start_smart },
    { label = "Toggle breakpoint", action = M.toggle_breakpoint },
    { label = "Conditional breakpoint", action = M.conditional_breakpoint },
    { label = "Logpoint", action = M.logpoint },
    { label = "Debug nearest test", action = M.debug_nearest_test },
    { label = "Debug file tests", action = M.debug_file_tests },
    { label = "Run to cursor", action = function() dap().run_to_cursor() end },
    { label = "Step over", action = function() dap().step_over() end },
    { label = "Step into", action = function() dap().step_into() end },
    { label = "Step out", action = function() dap().step_out() end },
    { label = "Scopes float", action = M.scopes },
    { label = "Frames float", action = M.frames },
    { label = "REPL", action = M.repl },
    { label = "Toggle UI", action = function() dapui().toggle() end },
    { label = "Debug info", action = M.info },
    { label = "Terminate", action = M.terminate },
  }

  vim.ui.select(items, {
    prompt = "Debug action",
    format_item = function(item)
      return item.label
    end,
  }, function(choice)
    if choice then
      choice.action()
    end
  end)
end

function M.setup_commands()
  vim.api.nvim_create_user_command("DebugPalette", M.palette, { desc = "Debug action palette" })
  vim.api.nvim_create_user_command("DebugStart", M.start_smart, { desc = "Start a smart debug session" })
  vim.api.nvim_create_user_command("DebugInfo", M.info, { desc = "Show DAP setup information" })
end

return M

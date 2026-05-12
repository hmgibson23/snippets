local M = {}

local function executable_prompt(default)
  return function()
    return vim.fn.input("Path to executable: ", default or (vim.fn.getcwd() .. "/"), "file")
  end
end

local function pick_process()
  return function()
    return require("dap.utils").pick_process()
  end
end

local function mason_package_path(package, ...)
  return vim.fs.joinpath(vim.fn.stdpath("data"), "mason", "packages", package, ...)
end

function M.javascript()
  return {
    {
      type = "pwa-node",
      request = "launch",
      name = "JS/TS: Launch current file",
      program = "${file}",
      cwd = "${workspaceFolder}",
      sourceMaps = true,
      protocol = "inspector",
      console = "integratedTerminal",
    },
    {
      type = "pwa-node",
      request = "launch",
      name = "JS/TS: npm test current file",
      runtimeExecutable = "npm",
      runtimeArgs = { "test", "--", "${file}" },
      cwd = "${workspaceFolder}",
      console = "integratedTerminal",
      sourceMaps = true,
    },
    {
      type = "pwa-node",
      request = "attach",
      name = "JS/TS: Attach to Node process",
      processId = pick_process(),
      cwd = "${workspaceFolder}",
      sourceMaps = true,
    },
    {
      type = "pwa-chrome",
      request = "attach",
      name = "Browser: Attach Chrome on 9222",
      port = 9222,
      webRoot = "${workspaceFolder}",
      sourceMaps = true,
    },
  }
end

function M.cpp()
  return {
    {
      name = "C/C++: Launch executable",
      type = "cppdbg",
      request = "launch",
      program = executable_prompt(vim.fn.getcwd() .. "/"),
      cwd = "${workspaceFolder}",
      stopAtEntry = false,
      setupCommands = {
        {
          text = "-enable-pretty-printing",
          description = "Enable pretty printing",
          ignoreFailures = true,
        },
      },
    },
    {
      name = "C/C++: Attach process",
      type = "cppdbg",
      request = "attach",
      processId = pick_process(),
      program = executable_prompt(vim.fn.getcwd() .. "/"),
      cwd = "${workspaceFolder}",
    },
  }
end

function M.go()
  return {
    {
      type = "go",
      name = "Go: Debug current file",
      request = "launch",
      program = "${file}",
    },
    {
      type = "go",
      name = "Go: Debug package",
      request = "launch",
      program = "${workspaceFolder}",
    },
  }
end

function M.python()
  return {
    {
      type = "python",
      request = "launch",
      name = "Python: Current file",
      program = "${file}",
      cwd = "${workspaceFolder}",
      console = "integratedTerminal",
      justMyCode = false,
    },
    {
      type = "python",
      request = "launch",
      name = "Python: Module",
      module = function()
        return vim.fn.input("Module: ")
      end,
      cwd = "${workspaceFolder}",
      console = "integratedTerminal",
      justMyCode = false,
    },
    {
      type = "python",
      request = "attach",
      name = "Python: Attach debugpy localhost:5678",
      connect = { host = "127.0.0.1", port = 5678 },
      justMyCode = false,
    },
  }
end

function M.apply(dap)
  dap.adapters.cppdbg = dap.adapters.cppdbg or {
    id = "cppdbg",
    type = "executable",
    command = mason_package_path("cpptools", "extension", "debugAdapters", "bin", "OpenDebugAD7"),
  }

  dap.configurations.python = M.python()

  local js = M.javascript()
  dap.configurations.javascript = js
  dap.configurations.typescript = vim.deepcopy(js)
  dap.configurations.javascriptreact = vim.deepcopy(js)
  dap.configurations.typescriptreact = vim.deepcopy(js)

  local cpp = M.cpp()
  dap.configurations.c = cpp
  dap.configurations.cpp = vim.deepcopy(cpp)

  dap.configurations.go = dap.configurations.go or M.go()
end

return M

local dap_filetypes = {
  "python",
  "javascript",
  "javascriptreact",
  "typescript",
  "typescriptreact",
  "c",
  "cpp",
  "go",
}

local function debugpy_python()
  local mason_debugpy = vim.fs.joinpath(vim.fn.stdpath("data"), "mason", "packages", "debugpy", "venv", "bin", "python")
  if vim.loop.fs_stat(mason_debugpy) then
    return mason_debugpy
  end
  if vim.fn.executable("python3") == 1 then
    return "python3"
  end
  return "python"
end

return {
  "mfussenegger/nvim-dap",
  lazy = true,
  ft = dap_filetypes,
  cmd = { "DebugPalette", "DebugStart", "DebugInfo", "DapContinue", "DapToggleBreakpoint" },
  dependencies = {
    "rcarriga/nvim-dap-ui",
    "nvim-neotest/nvim-nio",
    { "mfussenegger/nvim-dap-python" },
    "jay-babu/mason-nvim-dap.nvim",
    { "leoluz/nvim-dap-go" },
    { "jbyuki/one-small-step-for-vimkind", lazy = true },
    {
      "mxsdev/nvim-dap-vscode-js",
      dependencies = {
        {
          "microsoft/vscode-js-debug",
          build = "npm install --legacy-peer-deps && npm run compile",
        },
      },
    },
    { "theHamsta/nvim-dap-virtual-text" },
  },
  keys = {
    { "<leader>d", group = "[D]ebug" },
    { "<leader>d?", function() require("dap.actions").info() end, desc = "Debug info" },
    { "<leader>dP", function() require("dap.actions").palette() end, desc = "Debug palette" },
    { "<leader>dS", function() require("dap.actions").start_smart() end, desc = "Smart start" },
    { "<leader>dR", function() require("dap").run_to_cursor() end, desc = "Run to cursor" },
    { "<leader>dC", function() require("dap.actions").conditional_breakpoint() end, desc = "Conditional breakpoint" },
    { "<leader>dL", function() require("dap.actions").logpoint() end, desc = "Logpoint" },
    { "<leader>dU", function() require("dapui").toggle() end, desc = "Toggle UI" },
    { "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "Toggle breakpoint" },
    { "<leader>dc", function() require("dap.actions").continue_or_start() end, desc = "Continue / start" },
    { "<leader>dd", function() require("dap").disconnect() end, desc = "Disconnect" },
    { "<leader>de", function() require("dapui").eval() end, desc = "Evaluate" },
    { "<leader>dE", function() require("dap.actions").eval_input() end, desc = "Evaluate input" },
    { "<leader>df", function() require("dap.actions").frames() end, desc = "Frames float" },
    { "<leader>dh", function() require("dap.actions").hover() end, desc = "Hover variables" },
    { "<leader>di", function() require("dap").step_into() end, desc = "Step into" },
    { "<leader>do", function() require("dap").step_over() end, desc = "Step over" },
    { "<leader>dp", function() require("dap").pause() end, desc = "Pause" },
    { "<leader>dq", function() require("dap.actions").terminate() end, desc = "Terminate" },
    { "<leader>dr", function() require("dap.actions").repl() end, desc = "Toggle REPL" },
    { "<leader>ds", function() require("dap.actions").scopes() end, desc = "Scopes float" },
    { "<leader>dt", function() require("dap.actions").debug_nearest_test() end, desc = "Debug nearest test" },
    { "<leader>dT", function() require("dap.actions").debug_file_tests() end, desc = "Debug file tests" },
    { "<leader>du", function() require("dap").step_out() end, desc = "Step out" },
    { "<leader>dx", function() require("dap.actions").terminate() end, desc = "Terminate" },
    { "<leader>de", function() require("dapui").eval() end, desc = "Evaluate selection", mode = "v" },
    { "<F5>", function() require("dap.actions").continue_or_start() end, desc = "Debug continue/start" },
    { "<F9>", function() require("dap").toggle_breakpoint() end, desc = "Toggle breakpoint" },
    { "<F10>", function() require("dap").step_over() end, desc = "Step over" },
    { "<F11>", function() require("dap").step_into() end, desc = "Step into" },
    { "<F12>", function() require("dap").step_out() end, desc = "Step out" },
  },
  config = function()
    local dap = require("dap")
    local dapui = require("dapui")

    require("mason-nvim-dap").setup({
      automatic_installation = true,
      handlers = {},
      ensure_installed = { "python", "cppdbg", "delve" },
    })

    pcall(function()
      require("dap-python").setup(debugpy_python())
    end)

    pcall(function()
      require("dap-vscode-js").setup({
        debugger_path = vim.fs.joinpath(vim.fn.stdpath("data"), "lazy", "vscode-js-debug"),
        adapters = { "pwa-node", "pwa-chrome", "pwa-msedge", "node-terminal", "pwa-extensionHost" },
      })
    end)

    pcall(function()
      require("dap-go").setup({
        delve = { detached = vim.fn.has("win32") == 0 },
      })
    end)

    pcall(function()
      require("nvim-dap-virtual-text").setup({
        enabled = true,
        enabled_commands = true,
        highlight_changed_variables = true,
        highlight_new_as_changed = true,
        show_stop_reason = true,
        commented = true,
      })
    end)

    require("dap.configs").apply(dap)

    dapui.setup({
      icons = { expanded = "▾", collapsed = "▸", current_frame = "*" },
      mappings = {
        expand = { "<CR>", "<2-LeftMouse>" },
        open = "o",
        remove = "d",
        edit = "e",
        repl = "r",
        toggle = "t",
      },
      controls = {
        enabled = true,
        element = "repl",
        icons = {
          pause = "⏸",
          play = "▶",
          step_into = "⏎",
          step_over = "⏭",
          step_out = "⏮",
          step_back = "b",
          run_last = "▶▶",
          terminate = "⏹",
          disconnect = "⏏",
        },
      },
      floating = {
        border = "rounded",
        mappings = { close = { "q", "<Esc>" } },
      },
      layouts = {
        {
          elements = {
            { id = "scopes", size = 0.40 },
            { id = "watches", size = 0.20 },
            { id = "stacks", size = 0.25 },
            { id = "breakpoints", size = 0.15 },
          },
          size = 42,
          position = "left",
        },
        {
          elements = {
            { id = "repl", size = 0.65 },
            { id = "console", size = 0.35 },
          },
          size = 12,
          position = "bottom",
        },
      },
    })

    vim.api.nvim_set_hl(0, "DapBreak", { fg = "#e51400" })
    vim.api.nvim_set_hl(0, "DapStop", { fg = "#ffcc00" })
    vim.api.nvim_set_hl(0, "DapStoppedLine", { bg = "#3b4252" })

    local breakpoint_icons = vim.g.have_nerd_font
        and {
          Breakpoint = "",
          BreakpointCondition = "",
          BreakpointRejected = "",
          LogPoint = "",
          Stopped = "",
        }
        or {
          Breakpoint = "●",
          BreakpointCondition = "⊜",
          BreakpointRejected = "⊘",
          LogPoint = "◆",
          Stopped = "⭔",
        }

    for type, icon in pairs(breakpoint_icons) do
      local tp = "Dap" .. type
      local hl = (type == "Stopped") and "DapStop" or "DapBreak"
      vim.fn.sign_define(tp, { text = icon, texthl = hl, numhl = hl })
    end

    dap.listeners.after.event_initialized["dapui_config"] = function()
      dapui.open()
    end
    dap.listeners.before.event_terminated["dapui_config"] = function()
      dapui.close()
    end
    dap.listeners.before.event_exited["dapui_config"] = function()
      dapui.close()
    end

    require("dap.actions").setup_commands()
  end,
}

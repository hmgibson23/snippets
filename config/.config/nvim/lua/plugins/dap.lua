return {
  "mfussenegger/nvim-dap",
  lazy = true,
  event = "BufReadPre",
  keys = { [[<leader>d]] },
  dependencies = {
    "theHamsta/nvim-dap-virtual-text",
    "jay-babu/mason-nvim-dap.nvim",
    "rcarriga/nvim-dap-ui",
    {
      "mfussenegger/nvim-dap-python",
      config = function()
        require("dap-python").setup("~/.virtualenvs/debugpy/bin/python")
      end,
    },
    {
      "jay-babu/mason-nvim-dap.nvim",
      config = function()
        require("mason-nvim-dap").setup({
          automatic_setup = true,
          ensure_installed = { "stylua", "jq", "node2", "js", "chrome", "firefox", "js-debug-adapter" },
        })
      end,
    },
    { "leoluz/nvim-dap-go" },
    { "jbyuki/one-small-step-for-vimkind" },
    {
      "mxsdev/nvim-dap-vscode-js",
      dependencies = {
        "microsoft/vscode-js-debug",
        build = "npm install --legacy-peer-deps && npm run compile",
      },
      config = function()
        require("dap-vscode-js").setup({
          adapters = { "node2", "chrome", "firefox" }, -- Debug Node.js & browsers
        })
      end,
    },
    {
      "theHamsta/nvim-dap-virtual-text",
      config = function()
        require("nvim-dap-virtual-text").setup({
          enabled = true,                -- Enable virtual text
          enabled_commands = true,       -- Enable commands like `:DapVirtualTextEnable`
          highlight_changed_variables = true, -- Highlight changed values
          show_stop_reason = true,       -- Show why execution stopped
        })
      end,
    },
  },
  keys = {
    -- Integrated keymap from your request
    { "<leader>d",  group = "[D]AP" },
    { "<leader>dR", "<cmd>lua require'dap'.run_to_cursor()<cr>",                        desc = "Run to Cursor" },
    { "<leader>dE", "<cmd>lua require'dapui'.eval(vim.fn.input '[Expression] > ')<cr>", desc = "Evaluate Input" },
    {
      "<leader>dC",
      "<cmd>lua require'dap'.set_breakpoint(vim.fn.input '[Condition] > ')<cr>",
      desc = "Conditional Breakpoint",
    },
    { "<leader>dU", "<cmd>lua require'dapui'.toggle()<cr>",          desc = "Toggle UI" },
    { "<leader>db", "<cmd>lua require'dap'.step_back()<cr>",         desc = "Step Back" },
    { "<leader>dc", "<cmd>lua require'dap'.continue()<cr>",          desc = "Continue" },
    { "<leader>dd", "<cmd>lua require'dap'.disconnect()<cr>",        desc = "Disconnect" },
    { "<leader>de", "<cmd>lua require'dapui'.eval()<cr>",            desc = "Evaluate" },
    { "<leader>dg", "<cmd>lua require'dap'.session()<cr>",           desc = "Get Session" },
    { "<leader>dh", "<cmd>lua require'dap.ui.widgets'.hover()<cr>",  desc = "Hover Variables" },
    { "<leader>dS", "<cmd>lua require'dap.ui.widgets'.scopes()<cr>", desc = "Scopes" },
    { "<leader>di", "<cmd>lua require'dap'.step_into()<cr>",         desc = "Step Into" },
    { "<leader>do", "<cmd>lua require'dap'.step_over()<cr>",         desc = "Step Over" },
    { "<leader>dp", "<cmd>lua require'dap'.pause.toggle()<cr>",      desc = "Pause" },
    { "<leader>dq", "<cmd>lua require'dap'.close()<cr>",             desc = "Quit" },
    { "<leader>dr", "<cmd>lua require'dap'.repl.toggle()<cr>",       desc = "Toggle Repl" },
    { "<leader>ds", "<cmd>lua require'dap'.continue()<cr>",          desc = "Start" },
    { "<leader>dt", "<cmd>lua require'dap'.toggle_breakpoint()<cr>", desc = "Toggle Breakpoint" },
    { "<leader>dx", "<cmd>lua require'dap'.terminate()<cr>",         desc = "Terminate" },
    { "<leader>du", "<cmd>lua require'dap'.step_out()<cr>",          desc = "Step Out" },
    { "<leader>de", "<cmd>lua require'dapui'.eval()<cr>",            desc = "Evaluate",         mode = "v" },
  },
  config = function()
    local dap = require("dap")
    local dapui = require("dapui")

    -- Mason DAP setup
    require("mason-nvim-dap").setup({
      automatic_installation = true,
      handlers = {},
      ensure_installed = { "debugpy", "cpptools" },
    })

    -- DAP UI setup
    dapui.setup({
      icons = { expanded = "▾", collapsed = "▸", current_frame = "*" },
      controls = {
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
      layouts = {
        {
          elements = {
            { id = "scopes",      size = 0.25 },
            { id = "breakpoints", size = 0.25 },
            { id = "stacks",      size = 0.25 },
            { id = "watches",     size = 0.25 },
          },
          size = 40, -- Width of the left sidebar
          position = "left",
        },
        {
          elements = {
            { id = "repl", size = 1 }, -- Move REPL here
          },
          size = 40,             -- Width of the REPL
          position = "right",    -- Place REPL on the right
        },
      },
    })

    -- Change breakpoint icons
    vim.api.nvim_set_hl(0, "DapBreak", { fg = "#e51400" })
    vim.api.nvim_set_hl(0, "DapStop", { fg = "#ffcc00" })
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

    -- Setup event listeners
    dap.listeners.after.event_initialized["dapui_config"] = dapui.open
    dap.listeners.before.event_terminated["dapui_config"] = dapui.close
    dap.listeners.before.event_exited["dapui_config"] = dapui.close
  end,
}

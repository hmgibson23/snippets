local M = {}

-- Configure DAP breakpoints and signs
local function configure()
	local dap_breakpoint = {
		error = {
			text = "🟥",
			texthl = "DiagnosticError",
			linehl = "",
			numhl = "",
		},
		rejected = {
			text = "",
			texthl = "DiagnosticHint",
			linehl = "",
			numhl = "",
		},
		stopped = {
			text = "⭐️",
			texthl = "DiagnosticInfo",
			linehl = "DiagnosticUnderlineInfo",
			numhl = "DiagnosticInfo",
		},
	}

	vim.fn.sign_define("DapBreakpoint", dap_breakpoint.error)
	vim.fn.sign_define("DapStopped", dap_breakpoint.stopped)
	vim.fn.sign_define("DapBreakpointRejected", dap_breakpoint.rejected)
end

-- Configure extensions
local function configure_exts()
	-- Setup virtual text for nvim-dap
	require("nvim-dap-virtual-text").setup({
		commented = true, -- Display virtual text with comments
	})

	-- Setup DAP UI
	local dap, dapui = require("dap"), require("dapui")
	dapui.setup({}) -- Default configuration

	-- Automatically open/close DAP UI on debug events
	-- dap.listeners.after.event_initialized["dapui_config"] = function()
	-- 	dapui.open()
	-- end
	-- dap.listeners.before.event_terminated["dapui_config"] = function()
	-- 	dapui.close()
	-- end
	-- dap.listeners.before.event_exited["dapui_config"] = function()
	-- 	dapui.close()
	-- end
end

-- Configure individual debuggers
local function configure_debuggers()
	-- Load specific debugger configurations
	require("config.dap.python").setup()
	require("config.dap.rust").setup()
	-- Add other debuggers as needed, e.g., TypeScript, Lua
end

-- Setup Mason integration for managing DAP installations
local function configure_mason_dap()
	require("mason-nvim-dap").setup({
		ensure_installed = { "debugpy", "cpptools" }, -- Add adapters you want pre-installed
		handlers = {
			-- General handler for uncustomized adapters
			function(config)
				require("mason-nvim-dap").default_setup(config)
			end,
			-- Python-specific configuration
			python = function(config)
				config.adapters = {
					type = "executable",
					command = "/usr/bin/python3",
					args = { "-m", "debugpy.adapter" },
				}
				require("mason-nvim-dap").default_setup(config)
			end,
		},
	})
end

function M.setup()
	-- Core configuration
	configure()

	-- Extensions and keymaps
	configure_exts()
	require("config.dap.keymaps").setup()

	-- Mason DAP setup
	configure_mason_dap()

	-- Debugger-specific configurations
	configure_debuggers()
end

return M

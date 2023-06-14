local M = {}

local HOME = os.getenv("HOME")
local DEBUGGER_LOCATION = HOME .. "/.local/share/nvim/vscode-chrome-debug"

function M.setup()
	local dap = require("dap")
	local dap_vscode_ok, dap_vscode = pcall(require, "dap-vscode-js")
	if not dap_vscode_ok then
		vim.api.nvim_err_writeln("dap-vscode-js not installed!")
		return
	end

	dap.adapters.chrome = {
		type = "executable",
		command = "node",
		args = { DEBUGGER_LOCATION .. "/out/src/chromeDebug.js" },
	}
	dap.adapters.node2 = {
		type = "executable",
		command = "node",
		args = { vim.fn.stdpath("data") .. "/mason/packages/node-debug2-adapter/out/src/nodeDebug.js" },
	}
	dap_vscode.setup({
		adapters = { "pwa-node", "pwa-chrome", "pwa-msedge", "node-terminal", "pwa-extensionHost" },
		debugger_path = vim.fn.stdpath("data") .. "/mason/bin/js-debug-adapter", -- Path to VSCode Debugger
		debugger_cmd = { "js-debug-adapter" },
	})

	dap.configurations.javascript = {
		{
			type = "chrome",
			type = "pwa-chrome",
			request = "launch",
			name = "Chrome",
			webRoot = "${workspaceFolder}",
		},
	}

	dap.configurations.javascriptreact = {
		{
			type = "chrome",
			request = "attach",
			program = "${file}",
			cwd = vim.fn.getcwd(),
			sourceMaps = true,
			protocol = "inspector",
			port = 9222,
			webRoot = "${workspaceFolder}",
		},
	}

	dap.configurations.typescript = {
		{
			type = "node2",
			name = "node attach",
			request = "attach",
			program = "${file}",
			cwd = vim.fn.getcwd(),
			sourceMaps = true,
			protocol = "inspector",
		},
		{
			type = "chrome",
			type = "pwa-chrome",
			request = "launch",
			name = "Chrome",
			webRoot = "${workspaceFolder}",
		},
	}

	dap.configurations.typescriptreact = {
		{
			type = "chrome",
			request = "attach",
			program = "${file}",
			cwd = vim.fn.getcwd(),
			sourceMaps = true,
			protocol = "inspector",
			port = 9222,
			webRoot = "${workspaceFolder}",
		},
	}
end

return M

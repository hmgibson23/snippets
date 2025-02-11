local M = {}

local HOME = os.getenv("HOME")
local DEBUGGER_LOCATION = HOME .. "/.local/share/nvim/vscode-chrome-debug"

function M.setup()
	local dap_ok, dap = pcall(require, "dap")
	if not dap_ok then
		vim.notify("nvim-dap is not installed or failed to load.", vim.log.levels.ERROR)
		return
	end

	local mason_registry_ok, mason_registry = pcall(require, "mason-registry")
	if not mason_registry_ok then
		vim.notify("Mason registry is not installed or failed to load.", vim.log.levels.ERROR)
		return
	end

	local function get_js_debug()
		local package = mason_registry.get_package("js-debug-adapter")
		if not package then
			vim.notify("js-debug-adapter is not installed via Mason.", vim.log.levels.ERROR)
			return nil
		end
		local install_path = package:get_install_path()
		local debug_path = install_path .. "/js-debug/src/dapDebugServer.js"
		if vim.fn.filereadable(debug_path) == 0 then
			vim.notify("Debug server script not found: " .. debug_path, vim.log.levels.ERROR)
			return nil
		end
		return debug_path
	end

	local debug_path = get_js_debug()
	if not debug_path then
		vim.notify("Failed to set up js-debug adapter due to missing debug server script.", vim.log.levels.ERROR)
		return
	end

	for _, adapter in ipairs({ "pwa-node", "pwa-chrome", "pwa-msedge", "node-terminal", "pwa-extensionHost" }) do
		dap.adapters[adapter] = {
			type = "server",
			host = "localhost",
			port = "${port}",
			executable = {
				command = "node",
				args = { debug_path, "${port}" },
			},
		}
	end

	for _, language in ipairs({ "typescript", "javascript" }) do
		dap.configurations[language] = {
			{
				type = "pwa-node",
				request = "launch",
				name = "Launch Node file",
				program = "${file}",
				cwd = "${workspaceFolder}",
			},
			{
				type = "pwa-node",
				request = "attach",
				name = "Attach",
				processId = require("dap.utils").pick_process,
				cwd = "${workspaceFolder}",
			},
			{
				type = "pwa-node",
				request = "launch",
				name = "Debug Jest Tests",
				trace = true, -- include debugger info
				runtimeExecutable = "node",
				runtimeArgs = {
					"./node_modules/jest/bin/jest.js",
					"--runInBand",
				},
				rootPath = "${workspaceFolder}",
				cwd = "${workspaceFolder}",
				console = "integratedTerminal",
				internalConsoleOptions = "neverOpen",
			},
			{
				type = "pwa-chrome",
				name = "Attach - Remote Debugging",
				request = "attach",
				program = "${file}",
				cwd = vim.fn.getcwd(),
				sourceMaps = true,
				protocol = "inspector",
				port = 9222, -- Start Chrome google-chrome --remote-debugging-port=9222
				webRoot = "${workspaceFolder}",
			},
			{
				type = "pwa-chrome",
				name = "Launch Chrome",
				request = "launch",
				url = "http://localhost:3000", -- Change as per your setup
				webRoot = "${workspaceFolder}",
				userDataDir = "${workspaceFolder}/.vscode/vscode-chrome-debug-userdatadir",
			},
		}
	end

	vim.notify("DAP setup completed successfully.", vim.log.levels.INFO)
end

return M

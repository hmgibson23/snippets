local M = {}

function M.setup(_)
	require("dap-python").setup("python", {})
	local dap = require("dap")

	-- Configuration for attaching to a remote container
	table.insert(dap.configurations.python, {
		name = "Container FastAPI",
		type = "python",
		request = "attach",
		connect = {
			port = 5678,
			host = "127.0.0.1",
		},
		mode = "remote",
		cwd = vim.fn.getcwd(),
		pathMappings = {
			{
				localRoot = function()
					return vim.fn.input("Local code folder > ", vim.fn.getcwd(), "file")
				end,
				remoteRoot = function()
					return vim.fn.input("Container code folder > ", "/", "file")
				end,
			},
		},
	})

	-- Configuration for launching FastAPI with debugging
	table.insert(dap.configurations.python, {
		type = "python",
		name = "FastAPI debugger",
		request = "launch",
		module = "uvicorn",
		args = function()
			return {
				vim.fn.input("FastAPI app module > ", "src.app:app", "file"),
				"--use-colors",
			}
		end,
		console = "integratedTerminal",
		cwd = "${workspaceFolder}",
		env = { PYTHONPATH = "src" }, -- Properly set the environment variable
	})
end

return M

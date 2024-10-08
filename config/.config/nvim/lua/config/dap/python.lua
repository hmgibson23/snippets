local M = {}

function M.setup(_)
	require("dap-python").setup("python", {})
	table.insert(require("dap").configurations.python, {
		type = "python",
		request = "attach",
		connect = {
			port = 5678,
			host = "127.0.0.1",
		},
		mode = "remote",
		name = "Container Attach Debug",
		cwd = vim.fn.getcwd(),
		pathMappings = {
			{
				localRoot = function()
					return vim.fn.input("Local code folder > ", vim.fn.getcwd(), "file")
				end,
				remoteRoot = function()
					return vim.fn.input("Container code folder > ", "/", "file")
					-- "/fastapi", -- Wherever your Python code lives in the container.
				end,
			},
		},
	})

	table.insert(require("dap").configurations.python, {
		{
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
			options = {
				env = "PYTHONPATH=src",
			},
		},
	})
end

return M

local M = {}

function M.setup()
	-- local dap_install = require "dap-install"
	-- dap_install.config("codelldb", {})

	local dap = require("dap")
	local install_root_dir = vim.fn.stdpath("data") .. "/mason"
	local extension_path = install_root_dir .. "/packages/codelldb/extension/"
	local codelldb_path = extension_path .. "adapter/codelldb"

	dap.adapters.codelldb = {
		type = "server",
		port = "${port}",
		executable = {
			command = codelldb_path,
			args = { "--port", "${port}" },

			-- On windows you may have to uncomment this:
			-- detached = false,
		},
	}
	dap.adapters.lldb = {
		type = "executable",
		command = "/usr/bin/lldb-vscode", -- adjust as needed, must be absolute path
		name = "lldb",
		env = function()
			local variables = {}
			for k, v in pairs(vim.fn.environ()) do
				table.insert(variables, string.format("%s=%s", k, v))
			end
			return variables
		end,
	}
	dap.configurations.cpp = {
		{
			name = "Launch file",
			type = "codelldb",
			request = "launch",
			program = function()
				return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
			end,
			cwd = "${workspaceFolder}",
			stopOnEntry = true,
		},

		{
			name = "Launch lldb",
			type = "lldb",
			request = "launch",
			program = function()
				return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
			end,
			cwd = "${workspaceFolder}",
			stopOnEntry = false,
			args = {},
		},
	}

	dap.configurations.c = dap.configurations.cpp
	dap.configurations.rust = dap.configurations.cpp
end

return M

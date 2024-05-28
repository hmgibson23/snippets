local M = {}

M.setup = function()
	require("sniprun").setup({
		display = { "TerminalWithCode" },
		repl_enable = { "Python3_fifo" },
		interpreter_options = {
			Generic = {
				error_truncate = "long",
				sqlite3 = { -- any key name is ok
					supported_filetypes = { "sql" }, -- mandatory
					extension = ".sql", -- recommended, but not mandatory. Sniprun use this to create temporary files

					interpreter = "sqllite", -- interpreter or compiler (+ options if any)
					compiler = "", -- one of those MUST be non-empty
				},
			},
		},
		selected_interpreters = { "Python3_fifo", "Generic" },
	})
end

return M

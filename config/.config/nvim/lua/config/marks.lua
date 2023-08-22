local M = {}

function M.setup()
	require("marks").setup({
		default_mappings = true,
		builtin_marks = { ".", "<", ">", "^" },
		cyclic = true,
		refresh_interval = 250,
		sign_priority = { lower = 10, upper = 15, builtin = 8, bookmark = 20 },
		excluded_filetypes = {},
		bookmark_0 = {
			sign = "⚑",
			virt_text = "marked",
			annotate = false,
		},
	})
end
return M

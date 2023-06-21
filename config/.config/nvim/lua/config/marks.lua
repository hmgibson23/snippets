local M = {}
local whichkey = require("which-key")

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
	whichkey.register({
		x = {
			name = "Marks", -- optional group name
			l = { "<cmd>MarksListAllcr>", "List all marks" },
			t = { "<cmd>MarksToggleSigns<cr>", "Show marks" },
			b = { "<cmd>MarksListBuf<cr>", "List buffer marks" },
			g = { "<cmd>MarksListGlobal<cr>", "List global marks" },
			ql = { "<cmd>MarksQFListAll<cr>", "Qucikfix list all marks" },
			qg = { "<cmd>MarksQFListBuf<cr>", "Qucikfix list buffer marks" },
			qb = { "<cmd>MarksQFListGlobal<cr>", "Qucikfix list global marks" },
		},
	}, { prefix = "<leader>" })
end
return M

local M = {}

M.setup = function()
	require("sniprun").setup({
		display = { "NvimNotify" },
	})

	local whichkey = require('which-key')
	whichkey.register({
			r = {
				name = "SnipRun",
				r = { "<cmd>SnipRun<cr>", "SnipRun" },

			}

		}, { prefix = "<leader>" })

	whichkey.register({
			r = {
				name = "SnipRun",
				r = { "<cmd>SnipRun<cr>", "SnipRun" },

			}

		}, { mode = "v", prefix = "<leader>" })
end

return M

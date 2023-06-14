local M = {}

local overseer = require("overseer")
local whichkey = require("which-key")
function M.setup()
	overseer.setup()
	whichkey.register({
		o = {
			name = "Overseer", -- optional group name
			b = { "<cmd>OverseerBuild<cr>", "OverseerBuild" },
			r = { "<cmd>OverseerRun<cr>", "OverseerRun" },
			t = { "<cmd>OverseerToggle<cr>", "OverseerToggle" },
			c = { "<cmd>OverseerRunCmd<cr>", "OverseerRunCmd" },
		},
	}, { prefix = "<leader>" })
end

return M

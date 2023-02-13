local M = {}

local overseer = require("overseer")
local whichkey = require("which-key")
function M.setup() end

local keymap_l = {
	o = {
		name = "Overseer",
		d = { "<cmd>OverseerRun<CR>", "Overseer Run" },
		o = { "<cmd>OverseerOpen<CR>", "Overseer Open" },
		q = { "<cmd>OverseerQuickAction<CR>", "Overseer Quick Action" },
		a = { "<cmd>OverseerRunCmd<CR>", "Overseer Quick Action" },
	},
}

local o = { prefix = "<leader>" }
whichkey.register(keymap_l, o)
overseer.setup()
return M

local M = {}
local iron = require("iron.core")
local whichkey = require("which-key")

function M.setup()
	iron.setup({
		config = {
			should_map_plug = false,
			scratch_repl = true,
			repl_definition = {
				sh = {
					command = { "zsh" },
				},
			},
		},
		keymaps = {
			send_motion = "<space>sc",
			visual_send = "<space>sc",
			send_line = "<space>sl",
			cr = "<space>s<cr>",
			interrupt = "<space>s<space>",
			exit = "<space>sq",
			clear = "<space>cl",
		},
	})

	whichkey.register({
		i = {
			name = "Iron", -- optional group name
			i = { "<cmd>IronRepl<cr>", "Iron Repl" },
			f = { "<cmd>IronFocus<cr>", "Iron Focus" },
			h = { "<cmd>IronHide<cr>", "Iron Hide" },
		},
	}, { prefix = "<leader>" })
end

return M

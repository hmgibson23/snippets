local M = {}
local legendary = require("legendary")
local whichkey = require("which-key")
local l = require("legendary.integrations.which-key")

-- local function keymap(lhs, rhs, desc)
--   vim.keymap.set("n", lhs, rhs, { silent = true, desc = desc })
-- end

function M.setup()
	local keymap = {
		{ "<leader>d", group = "DAP" },
		{ "<leader>dR", "<cmd>lua require'dap'.run_to_cursor()<cr>", desc = "Run to Cursor" },
		{ "<leader>dE", "<cmd>lua require'dapui'.eval(vim.fn.input '[Expression] > ')<cr>", desc = "Evaluate Input" },
		{
			"<leader>dC",
			"<cmd>lua require'dap'.set_breakpoint(vim.fn.input '[Condition] > ')<cr>",
			desc = "Conditional Breakpoint",
		},
		{ "<leader>dU", "<cmd>lua require'dapui'.toggle()<cr>", desc = "Toggle UI" },
		{ "<leader>db", "<cmd>lua require'dap'.step_back()<cr>", desc = "Step Back" },
		{ "<leader>dc", "<cmd>lua require'dap'.continue()<cr>", desc = "Continue" },
		{ "<leader>dd", "<cmd>lua require'dap'.disconnect()<cr>", desc = "Disconnect" },
		{ "<leader>de", "<cmd>lua require'dapui'.eval()<cr>", desc = "Evaluate" },
		{ "<leader>dg", "<cmd>lua require'dap'.session()<cr>", desc = "Get Session" },
		{ "<leader>dh", "<cmd>lua require'dap.ui.widgets'.hover()<cr>", desc = "Hover Variables" },
		{ "<leader>dS", "<cmd>lua require'dap.ui.widgets'.scopes()<cr>", desc = "Scopes" },
		{ "<leader>di", "<cmd>lua require'dap'.step_into()<cr>", desc = "Step Into" },
		{ "<leader>do", "<cmd>lua require'dap'.step_over()<cr>", desc = "Step Over" },
		{ "<leader>dp", "<cmd>lua require'dap'.pause.toggle()<cr>", desc = "Pause" },
		{ "<leader>dq", "<cmd>lua require'dap'.close()<cr>", desc = "Quit" },
		{ "<leader>dr", "<cmd>lua require'dap'.repl.toggle()<cr>", desc = "Toggle Repl" },
		{ "<leader>ds", "<cmd>lua require'dap'.continue()<cr>", desc = "Start" },
		{ "<leader>dt", "<cmd>lua require'dap'.toggle_breakpoint()<cr>", desc = "Toggle Breakpoint" },
		{ "<leader>dx", "<cmd>lua require'dap'.terminate()<cr>", desc = "Terminate" },
		{ "<leader>du", "<cmd>lua require'dap'.step_out()<cr>", desc = "Step Out" },
		{ "<leader>de", "<cmd>lua require'dapui'.eval()<cr>", desc = "Evaluate", mode = "v" },
	}
	whichkey.add(keymap)
	-- l.bind_whichkey(keymap)
end

return M

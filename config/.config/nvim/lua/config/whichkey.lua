local M = {}

M.setup = function()
	require("which-key").setup()
	local whichkey = require("which-key")
	whichkey.register({
		k = {
			name = "Test",
			a = { "<cmd>lua require('neotest').run.attach()<cr>", "Attach" },
			f = { "<cmd>lua require('neotest').run.run(vim.fn.expand('%'))<cr>", "Run File" },
			F = {
				"<cmd>lua require('neotest').run.run({vim.fn.expand('%'), strategy = 'dap'})<cr>",
				"Debug File",
			},
			l = { "<cmd>lua require('neotest').run.run_last()<cr>", "Run Last" },
			L = { "<cmd>lua require('neotest').run.run_last({ strategy = 'dap' })<cr>", "Debug Last" },
			n = { "<cmd>lua require('neotest').run.run()<cr>", "Run Nearest" },
			N = { "<cmd>lua require('neotest').run.run({strategy = 'dap'})<cr>", "Debug Nearest" },
			O = { "<cmd>lua require('neotest').output.open({ enter = true })<cr>", "Output Float" },
			o = { "<cmd>lua require('neotest').output_panel.toggle()<cr>", "Output" },
			S = { "<cmd>lua require('neotest').run.stop()<cr>", "Stop" },
			s = { "<cmd>lua require('neotest').summary.toggle()<cr>", "Summary" },
			p = { "<Plug>PlenaryTestFile", "PlenaryTestFile" },
			v = { "<cmd>TestVisit<cr>", "Visit" },
			x = { "<cmd>TestSuite<cr>", "Suite" },
			t = {
				name = "Test",
				l = { "<cmd>TestLast<cr>", "Run Last" },
				n = { "<cmd>TestNearest<cr>", "Run Nearest" },
			},
		},
	}, { prefix = "<leader>" })
end

return M

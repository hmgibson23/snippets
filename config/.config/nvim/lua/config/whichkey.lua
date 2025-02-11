local M = {}

M.setup = function()
	require("which-key").setup()
	local whichkey = require("which-key")
	whichkey.setup({
		preset = "helix",
		window = {
			border = "single", -- border style
			position = "bottom", -- position of the window
			margin = { 1, 0, 1, 0 }, -- margin around the window
			padding = { 2, 2, 2, 2 }, -- padding inside the window
			winblend = 0, -- transparency of the window
		},
		layout = {
			height = { min = 10, max = 25 }, -- min and max height of the columns
			width = { min = 20, max = 50 }, -- min and max width of the columns
			spacing = 3, -- spacing between columns
			align = "left", -- align columns left, center or right
		},
		ignore_missing = true, -- whether to ignore missing key mappings
		show_help = true, -- show help message on the first time
	})

	whichkey.add({
		-- Bufferline
		{ "gt", "<cmd>BufferLineCycleNext<cr>", desc = "Next buffer" },
		{ "gp", "<cmd>BufferLinePick<cr>", desc = "Pick buffer" },
		-- Venv
		{ "<leader>lv", group = "Venv" },
		{ "<leader>lvs", "<cmd>VenvSelect<cr>", desc = "Select" },

		-- General key mappings for Neotest
		{ "<leader>k", group = "Neotest" },

		{ "<leader>tp", "<cmd>TSPlay<cr>", desc = "TSPlay" },
		{ "<leader>kt", group = "Test Commands" },
		{ "<leader>ka", "<cmd>lua require('neotest').run.attach()<cr>", desc = "Attach" },
		{
			"<leader>kA",
			"<cmd>lua require('neotest').run.run({ suite = true })<cr>",
			desc = "Run all",
		},
		{
			"<leader>kf",
			"<cmd>lua require('neotest').run.run(vim.fn.expand('%'))<cr>",
			desc = "Run file",
		},
		{
			"<leader>kF",
			"<cmd>lua require('neotest').run.run({vim.fn.expand('%'), strategy = 'dap'})<cr>",
			desc = "Debug File",
		},
		{ "<leader>kl", "<cmd>lua require('neotest').run.run_last()<cr>", desc = "Run last" },
		{ "<leader>kN", "<cmd>lua require('neotest').run.run({strategy = 'dap'})<cr>", desc = "Debug nearest" },
		{ "<leader>kO", "<cmd>lua require('neotest').output.open({ enter = true })<cr>", desc = "Output float" },
		{ "<leader>ko", "<cmd>lua require('neotest').output_panel.toggle()<cr>", desc = "Output Panel" },
		{ "<leader>kS", "<cmd>lua require('neotest').run.stop()<cr>", desc = "Stop" },
		{ "<leader>ks", "<cmd>lua require('neotest').summary.toggle()<cr>", desc = "Summary" },
		{ "<leader>kp", "<Plug>PlenaryTestFile", desc = "Plenary test file" },
		{ "<leader>kv", "<cmd>TestVisit<cr>", desc = "Visit Test File" },
		{ "<leader>kx", "<cmd>TestSuite<cr>", desc = "Run Test Suite" },
		{ "<leader>ktl", "<cmd>TestLast<cr>", desc = "Run Last" },
		{ "<leader>ktn", "<cmd>TestNearest<cr>", desc = "Run Nearest" },

		-- Key mappings for SnipRun
		{ "<leader>r", group = "SnipRun" },
		{ "<leader>rr", "<cmd>SnipRun<cr>", desc = "Run" },
		{ "<leader>rf", "<cmd>%SnipRun<cr>", desc = "File" },
		{ "<leader>rf", "<cmd>Run<cr>", desc = "Officer" },
		{
			"<leader>rj",
			"<cmd>lua require'sniprun'.setup({selected_interpreters = {'Python3_jupyter'}})<cr>",
			desc = "Jupyter",
		},

		{ "<leader>S", group = "Scratch" },

		-- Parrot
		{ "<leader>w", group = "Parrot", icon = "", mode = { "n", "v" } },
		{ "<leader>wtn", "<cmd>PrtChatNew<cr>", desc = "New chat", icon = "󰈙", mode = { "n", "v" } },
		{ "<leader>wai", "<cmd>PrtImplement<cr>", desc = "Implement", icon = "λ", mode = { "n", "v" } },
		{ "<leader>wa", "<cmd>PrtAsk<cr>", desc = "Ask" },
		{ "<leader>wb", "<cmd>PrtAppend<cr>", desc = "Append" },
		{ "<leader>wtt", "<cmd>PrtChatToggle<cr>", desc = "Toggle chat" },
		{ "<leader>wtp", "<cmd>PrtChatPaste<cr>", desc = "Paste chat" },
		{ "<leader>wtd", "<cmd>PrtChatDelete<cr>", desc = "Delete chat" },
		{ "<leader>wtr", "<cmd>PrtChatRespond<cr>", desc = "Respond to chat" },
		{ "<leader>wtf", "<cmd>PrtChatFind<cr>", desc = "Find chat" },
		{ "<leader>we", "<cmd>PrtENew<cr>", desc = "New E" },
		{ "<leader>wp", "<cmd>PrtPopup<cr>", desc = "Popup" },
		{ "<leader>wcc", "<cmd>PrtContext<cr>", desc = "Context" },
		{ "<leader>wcf", "<cmd>PrtFullContext<cr>", desc = "Full Context" },
		{ "<leader>wcm", "<cmd>PrtMultiContext<cr>", desc = "Multi Context" },
		{ "<leader>wr", "<cmd>PrtRewrite<cr>", desc = "Rewrite", mode = { "n", "v" } },
		{ "<leader>ws", "<cmd>PrtStop<cr>", desc = "Stop" },
		{ "<leader>wu", "<cmd>PrtUnitTests<cr>", desc = "Unit tests", mode = { "n", "v" } },
		{ "<leader>wl", "<cmd>PrtFixBugs<cr>", desc = "Fix bugs", mode = { "n", "v" } },
		{ "<leader>wd", "<cmd>PrtDebug<cr>", desc = "Debug", mode = { "n", "v" } },
		-- Vim Buffers
		{ "<leader>u", group = "Vim" },
		{ "<leader>up", "<cmd>bp<cr>", desc = "Previous buffer" },
		{ "<leader>un", "<cmd>bn<cr>", desc = "Next buffer" },
		{ "<leader>ud", "<cmd>bdelete<cr>", desc = "Delete buffer" },

		-- YaREPL
		{ "<leader>y", group = "YaREPL" },
		{ "<leader>ys", "<cmd>REPLStart<cr>", desc = "Start REPL" },
		{ "<leader>yl", "<cmd>REPLSendLine<cr>", desc = "Send Line" },
		{ "<leader>yo", "<cmd>REPLSendOperator<cr>", desc = "Send Operator" },
		{ "<leader>yt", "<cmd>REPLHideOrFocus<cr>", desc = "Toggle REPL" },

		-- Iron
		{ "<leader>i", group = "Iron" },
		{ "<leader>ii", "<cmd>IronRepl<cr>", desc = "Open REPL" },
		{ "<leader>if", "<cmd>IronFocus<cr>", desc = "Focus REPL" },
		{ "<leader>ih", "<cmd>IronHide<cr>", desc = "Hide REPL" },
		{ "<leader>ir", "<cmd>IronRestart<cr>", desc = "Restart REPL" },
		{ "<leader>iF", ":lua require('iron.core').send_file()<cr>", desc = "Send File" },
		{ "<leader>il", ":lua require('iron.core').send_line('r')<cr>", desc = "Send Line" },

		-- Overseer
		{ "<leader>o", group = "Overseer" },
		{ "<leader>ob", "<cmd>OverseerBuild<cr>", desc = "Build", icon = "" },
		{ "<leader>or", "<cmd>OverseerRun<cr>", desc = "Run", icon = "" },
		{ "<leader>ot", "<cmd>OverseerToggle<cr>", desc = "Toggle" },
		{ "<leader>oc", "<cmd>OverseerRunCmd<cr>", desc = "Run Command" },

		-- Marks
		{ "<leader>x", group = "Marks" },
		{ "<leader>xl", "<cmd>MarksListAll<cr>", desc = "List All Marks" },
		{ "<leader>xt", "<cmd>MarksToggleSigns<cr>", desc = "Toggle Mark Signs" },
		{ "<leader>xb", "<cmd>MarksListBuf<cr>", desc = "List Buffer Marks" },
		{ "<leader>xg", "<cmd>MarksListGlobal<cr>", desc = "List Global Marks" },
		{ "<leader>xql", "<cmd>MarksQFListAll<cr>", desc = "Quickfix List All Marks" },
		{ "<leader>xqg", "<cmd>MarksQFListBuf<cr>", desc = "Quickfix List Buffer Marks" },
		{ "<leader>xqb", "<cmd>MarksQFListGlobal<cr>", desc = "Quickfix List Global Marks" },

		-- Spectacle
		{ "<leader>s", group = "Spectacle" },
		{ "<leader>ss", "<cmd>Scratch<cr>", desc = "Save session" },

		-- Legendary
		{ "<leader>L", "<cmd>Legendary<cr>", desc = "Legendary" },

		-- Molten
		{ "<leader>b", group = "Molten" },
		{ "<leader>bi", "<cmd>MoltenInit<cr>", desc = "Initialize Molten" },
		{ "<leader>bl", "<cmd>MoltenEvaluateLine<cr>", desc = "Evaluate Line" },
		{ "<leader>ba", "<cmd>MoltenReevaluateAll<cr>", desc = "Reevaluate All" },
		{ "<leader>bo", "<cmd>MoltenEvaluateOperator<cr>", desc = "Evaluate Operator" },
		{ "<leader>bc", "<cmd>MoltenReevaluateCell<cr>", desc = "Reevaluate Cell" },
		{ "<leader>bd", "<cmd>MoltenInterrupt<cr>", desc = "Interrupt" },
		{ "<leader>bs", "<cmd>MoltenSave<cr>", desc = "Save Output" },
		{ "<leader>bh", "<cmd>MoltenHideOutput<cr>", desc = "Hide Output" },
		{ "<leader>bg", "<cmd>MoltenShowOutput<cr>", desc = "Show Output" },
		{ "<leader>bn", "<cmd>MoltenNext<cr>", desc = "Next Output" },
		{ "<leader>bp", "<cmd>MoltenPrev<cr>", desc = "Previous Output" },
		{ "<leader>be", "<cmd>noautocmd MoltenEnterOutput<cr>", desc = "Enter Output" },

		-- Project
		{ "<leader>p", group = "Project" },
		{ "<leader>pf", "<cmd>Telescope projections<cr>", desc = "Find Project File" },
		{ "<leader>pap", "<cmd>lua require('projections.path').project_from_path()<cr>", desc = "Add project" },

		{ "gbp", "previous buffer" },
		{ "gbn", "next buffer" },

		-- Grapple
		{ "<leader>g", group = "Grapple" },
		{ "<leader>gm", "<cmd>Grapple toggle<cr>", desc = "Grapple toggle tag" },
		{ "<leader>gM", "<cmd>Grapple toggle_tags<cr>", desc = "Grapple open tags window" },
		{ "<leader>ge", "<cmd>Grapple cycle_tags prev<cr>", desc = "Grapple cycle previous tag" },

		-- Quarto
		{ "<leader>q", group = "Quarto" },
		{ "<leader>qc", "<cmd>lua require('quarto.runner').run_cell()<cr>", desc = "Run Cell" },
		{ "<leader>qs", "<cmd>QuartoActivate<cr>", desc = "Activate Quarto" },
		{ "<leader>qa", "<cmd>lua require('quarto.runner').run_above()<cr>", desc = "Run Cell and Above" },
		{ "<leader>qA", "<cmd>lua require('quarto.runner').run_all()<cr>", desc = "Run All Cells" },
		{ "<leader>qp", "<cmd>QuartoPreview<cr>", desc = "Preview Quarto" },

		-- YaREPL (Visual)
		{ ":y", group = "YaREPL (Visual)", mode = "v" },
		{ ":ys", "<cmd>REPLSendVisual<cr>", desc = "Send Visual Selection", mode = "v" },

		-- SnipRun (Visual)
		{ "<leader>r", group = "SnipRun (Visual)", mode = "v" },
		{ "<leader>rr", "<cmd>SnipRun<cr>", desc = "Run Snippet", mode = "v" },

		-- Molten (Visual)
		{ "<leader>b", group = "Molten", mode = "v" },
		{ "<leader>bv", "<cmd>MoltenEvaluateVisual<CR>gv", desc = "Evaluate Selection", mode = "v" },
	})

	-- Dynamic key mapping based on file type
	function CodeRunner()
		local bufnr = vim.api.nvim_get_current_buf()
		local ft = vim.api.nvim_buf_get_option(bufnr, "filetype")
		local fname = vim.fn.expand("%:p:t")
		local keymap_c = {} -- Normal mode key mappings
		local keymap_c_v = {} -- Visual mode key mappings

		-- Define key mappings based on filetype
		if ft == "python" then
			keymap_c = {
				name = "Code",
				i = { "<cmd>cexpr system('refurb --quiet ' . shellescape(expand('%'))) | copen<cr>", "Inspect Code" },
				r = {
					"<cmd>update<cr><cmd>lua require('utils.term').open_term([[python3 ']]..fname..[[']])<cr>",
					"Run Python File",
				},
			}
			keymap_c_v = {
				name = "Python",
				r = { "<cmd>lua require('utils.term').open_term([[python3 ']]..fname..[[']])<cr>", "Run Python File" },
			}
		end

		-- Register dynamic key mappings
		-- whichkey.register(keymap_c, { prefix = "<leader>" })
		-- whichkey.register(keymap_c_v, { mode = "v", prefix = "<leader>" })
	end

	CodeRunner()
end

return M

local M = {}

M.setup = function()
	require("which-key").setup()
	local whichkey = require("which-key")
	whichkey.register({
		k = {
			name = "Test",
			a = { "<cmd>lua require('neotest').run.attach()<cr>", "Attach" },
			A = { "<cmd>lua require('neotest').run.run({ suite = true })<cr>", "All" },
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

	whichkey.register({
		r = {
			name = "SnipRun",
			r = { "<cmd>SnipRun<cr>", "SnipRun" },
			f = { "<cmd>%SnipRun<cr>", "SnipRun file" },
			j = {
				"<cmd>lua require'sniprun'.setup({selected_interpreters = {'Python3_jupyter'}})<cr>",
				"Enable Jupyter",
			},
		},
	}, { prefix = "<leader>" })

	whichkey.register({
		y = {
			name = "YaREPL",
			s = { "<cmd>REPLStart<cr>", "Start Repl" },
			l = { "<cmd>REPLSendLine<cr>", "Send Line" },
			o = { "<cmd>REPLSendOperator<cr>", "Send Operator" },
			t = { "<cmd>REPLHideOrFocus<cr>", "Toggle" },
		},
	}, { prefix = "<leader>" })

	whichkey.register({
		y = {
			name = "YaREPL",
			s = { "<cmd>REPLSendVisual<cr>", "Send visual" },
		},
	}, { mode = "v", prefix = ":" })

	whichkey.register({
		r = {
			name = "SnipRun",
			r = { "<cmd>SnipRun<cr>", "SnipRun" },
		},
		b = {
			v = { "<cmd>MoltenEvaluateVisual<CR>gv", "Molten visual" },
		},
	}, { mode = "v", prefix = "<leader>" })

	whichkey.register({
		i = {
			name = "Iron", -- optional group name
			i = { "<cmd>IronRepl<cr>", "Iron Repl" },
			f = { "<cmd>IronFocus<cr>", "Iron Focus" },
			h = { "<cmd>IronHide<cr>", "Iron Hide" },
		},
		o = {
			name = "Overseer", -- optional group name
			b = { "<cmd>OverseerBuild<cr>", "OverseerBuild" },
			r = { "<cmd>OverseerRun<cr>", "OverseerRun" },
			t = { "<cmd>OverseerToggle<cr>", "OverseerToggle" },
			c = { "<cmd>OverseerRunCmd<cr>", "OverseerRunCmd" },
		},
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
		g = {
			name = "GitSigns", -- optional group name
			g = { "<cmd>GitSigns<cr>", "GitSigns" },
		},
		f = {
			name = "Telescope", -- optional group name
			f = { "<cmd>Telescope find_files<cr>", "Find Files" },
			b = { "<cmd>Telescope buffers<cr>", "Buffers" },
			g = { "<cmd>Telescope live_grep<cr>", "Grep" },
			r = { "<cmd>Telescope registers<cr>", "Registers" },
			t = { "<cmd>Telescope treesitter<cr>", "Treesitter" },
			o = { "<cmd>Telescope oldfiles<cr>", "Old Files" },
			m = { "<cmd>Telescope man_pages<cr>", "Man Pages" },
			w = { "<cmd>Telescope whaler<cr>", "Whaler" },
		},

		b = {
			name = "Molten",
			i = { "<cmd>MoltenInit<cr>", "Init" },
			l = { "<cmd>MoltenEvaluateLine<cr>", "Molten eval line" },
			a = { "<cmd>MoltenReevaluateAll<cr>", "Molten eval all" },
			o = { "<cmd>MoltenEvaluateOperator<cr>", "Molten eval operator" },
			c = { "<cmd>MoltenReevaluateCell<cr>", "Molten eval cell" },
			d = { "<cmd>MoltenInterrupt<cr>", "Molten interrupt" },
			s = { "<cmd>MoltenSave<cr>", "Molten save" },
			h = { "<cmd>MoltenHideOutput<cr>", "Molten hide output" },
			g = { "<cmd>MoltenShowOutput<cr>", "Molten show output" },
			n = { "<cmd>MoltenNext<cr>", "Molten next" },
			p = { "<cmd>MoltenPrev<cr>", "Molten prev" },
			e = { "<cmd>noautocmd MoltenEnterOutput<cr>", "Molten show output" },
		},
		q = {
			name = "Quarto",
			c = { "<cmd>lua require('quarto.runner').run_cell()<cr>", "Run cell" },
			s = { "<cmd>QuartoActivate<cr>", "Activate" },
			a = { "<cmd>lua require('quarto.runner').run_above()<cr>", "Run cell and above" },
			A = { "<cmd>lua require('quarto.runner').run_all()<cr>", "Run all" },
		},
	}, { prefix = "<leader>", silent = true, noremap = true })

	function CodeRunner()
		local bufnr = vim.api.nvim_get_current_buf()
		local ft = vim.api.nvim_buf_get_option(bufnr, "filetype")
		local fname = vim.fn.expand("%:p:t")
		local keymap_c = {} -- normal key map
		local keymap_c_v = {} -- visual key map

		if ft == "python" then
			keymap_c = {
				name = "Code",
				-- r = { "<cmd>update<CR><cmd>exec '!python3' shellescape(@%, 1)<cr>", "Run" },
				-- r = { "<cmd>update<CR><cmd>TermExec cmd='python3 %'<cr>", "Run" },
				i = { "<cmd>cexpr system('refurb --quiet ' . shellescape(expand('%'))) | copen<cr>", "Inspect" },
				r = {
					"<cmd>update<cr><cmd>lua require('utils.term').open_term([[python3 ]] .. vim.fn.shellescape(vim.fn.getreg('%'), 1), {direction = 'float'})<cr>",
					"Run",
				},
				m = { "<cmd>TermExec cmd='nodemon -e py %'<cr>", "Monitor" },
			}
		elseif ft == "lua" then
			keymap_c = {
				name = "Code",
				r = { "<cmd>luafile %<cr>", "Run" },
			}
		elseif ft == "rust" then
			keymap_c = {
				name = "Code",
				r = { "<cmd>execute 'Cargo run' | startinsert<cr>", "Run" },
				D = { "<cmd>RustDebuggables<cr>", "Debuggables" },
				h = { "<cmd>RustHoverActions<cr>", "Hover Actions" },
				R = { "<cmd>RustRunnables<cr>", "Runnables" },
			}
		elseif ft == "go" then
			keymap_c = {
				name = "Code",
				r = { "<cmd>GoRun<cr>", "Run" },
			}
		elseif ft == "typescript" or ft == "typescriptreact" or ft == "javascript" or ft == "javascriptreact" then
			keymap_c = {
				name = "Code",
				o = { "<cmd>TypescriptOrganizeImports<cr>", "Organize Imports" },
				r = { "<cmd>TypescriptRenameFile<cr>", "Rename File" },
				i = { "<cmd>TypescriptAddMissingImports<cr>", "Import Missing" },
				F = { "<cmd>TypescriptFixAll<cr>", "Fix All" },
				u = { "<cmd>TypescriptRemoveUnused<cr>", "Remove Unused" },
				R = { "<cmd>lua require('config.test').javascript_runner()<cr>", "Choose Test Runner" },
				-- s = { "<cmd>2TermExec cmd='yarn start'<cr>", "Yarn Start" },
				-- t = { "<cmd>2TermExec cmd='yarn test'<cr>", "Yarn Test" },
			}
		elseif ft == "java" then
			keymap_c = {
				name = "Code",
				o = { "<cmd>lua require'jdtls'.organize_imports()<cr>", "Organize Imports" },
				v = { "<cmd>lua require('jdtls').extract_variable()<cr>", "Extract Variable" },
				c = { "<cmd>lua require('jdtls').extract_constant()<cr>", "Extract Constant" },
				t = { "<cmd>lua require('jdtls').test_class()<cr>", "Test Class" },
				n = { "<cmd>lua require('jdtls').test_nearest_method()<cr>", "Test Nearest Method" },
				s = { "<cmd>JdtsJshell<cr>", "Jshell" },
			}
			keymap_c_v = {
				name = "Code",
				v = { "<cmd>lua require('jdtls').extract_variable(true)<cr>", "Extract Variable" },
				c = { "<cmd>lua require('jdtls').extract_constant(true)<cr>", "Extract Constant" },
				m = { "<cmd>lua require('jdtls').extract_method(true)<cr>", "Extract Method" },
			}
		end

		if fname == "package.json" then
			keymap_c.v = { "<cmd>lua require('package-info').show()<cr>", "Show Version" }
			keymap_c.c = { "<cmd>lua require('package-info').change_version()<cr>", "Change Version" }
		end

		if fname == "Cargo.toml" then
			keymap_c.u = { "<cmd>lua require('crates').upgrade_all_crates()<cr>", "Upgrade All Crates" }
		end

		if next(keymap_c) ~= nil then
			local k = { c = keymap_c }
			local o = { mode = "n", silent = true, noremap = true, buffer = bufnr, prefix = "<leader>", nowait = true }
			whichkey.register(k, o)
			-- legendary.bind_whichkey(k, o, false)
		end

		if next(keymap_c_v) ~= nil then
			local k = { c = keymap_c_v }
			local o = { mode = "v", silent = true, noremap = true, buffer = bufnr, prefix = "<leader>", nowait = true }
			whichkey.register(k, o)
			-- legendary.bind_whichkey(k, o, false)
		end
	end
	vim.api.nvim_create_autocmd("FileType", {
		pattern = "*",
		callback = function()
			vim.schedule(CodeRunner)
		end,
	})
end

return M

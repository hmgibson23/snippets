local M = {}

function M.setup()
	local status_ok, projections = pcall(require, "projections")
	if not status_ok then
		return
	end

	projections.setup({
		workspaces = { -- Default workspaces to search for
			{ "~/git", {} },
			{ "~/git/newsuk", {} },
			{ "~/git/flowers", {} },
			{ "~/.config/", { ".projections_project" } },
		},
	})

	vim.opt.sessionoptions:append("localoptions")
	-- Bind <leader>fp to Telescope projections
	require("telescope").load_extension("projections")
	vim.keymap.set("n", "<leader>pp", function()
		vim.cmd("Telescope projections")
	end)

	-- Autostore session on VimExit
	local Session = require("projections.session")
	vim.api.nvim_create_autocmd({ "VimLeavePre" }, {
		callback = function()
			Session.store(vim.loop.cwd())
		end,
	})

	-- Switch to project if vim was started in a project dir
	local switcher = require("projections.switcher")
	vim.api.nvim_create_autocmd({ "VimEnter" }, {
		callback = function()
			if vim.fn.argc() == 0 then
				switcher.switch(vim.loop.cwd())
			end
		end,
	})

	local Workspace = require("projections.workspace")
	-- Add workspace command
	vim.api.nvim_create_user_command("AddWorkspace", function()
		Workspace.add(vim.loop.cwd())
	end, {})
end

return M

local M = {}
local function get_cwd_as_name()
	local dir = vim.fn.getcwd(0)
	return dir:gsub("[^A-Za-z0-9]", "_")
end

function M.setup()
	local status_ok, projections = pcall(require, "projections")
	if not status_ok then
		return
	end

	-- Configure projections
	projections.setup({
		workspaces = {
			"~/git",
			"~/git/tulip",
			"~/git/newsuk",
		},
		patterns = { ".git", ".svn", ".hg" },
		workspaces_file = "~/.local/share/nvim/projections_workspaces.json",
		store_hooks = {
			pre = function()
				local ook, overseer = pcall(require, "overseer")
				if not ook then
					return
				end
				overseer.save_task_bundle(
					get_cwd_as_name(),
					-- Passing nil will use config.opts.save_task_opts. You can call list_tasks() explicitly and
					-- pass in the results if you want to save specific tasks.
					nil,
					{ on_conflict = "overwrite" } -- Overwrite existing bundle, if any
				)
			end,
		},
		restore_hooks = {
			post = function()
				local ook, overseer = pcall(require, "overseer")
				if not ook then
					return
				end
				overseer.load_task_bundle(get_cwd_as_name(), { ignore_missing = true })
			end,
		},
	})

	-- Set session options
	vim.opt.sessionoptions:append("localoptions")

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

	vim.api.nvim_create_user_command("AddWorkspace", function() end, {})
end

return M

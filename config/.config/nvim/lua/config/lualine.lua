local M = {}

local project_name_display = function()
	local projections_available, Session = pcall(require, "projections.session")
	if projections_available then
		local info = Session.info(vim.loop.cwd())
		if info ~= nil then
			-- local session_file_path = tostring(info.path)
			-- local project_workspace_patterns = info.project.workspace.patterns
			-- local project_workspace_path = tostring(info.project.workspace)
			local project_name = info.project.name
			return "🮮 " .. project_name -- you can also just return an icon here.
		end
	end
	return vim.fs.basename(vim.loop.cwd())
end

local colors = {
	blue = "#80a0ff",
	cyan = "#79dac8",
	black = "#080808",
	white = "#c6c6c6",
	red = "#ff5189",
	violet = "#d183e8",
	grey = "#303030",
}

local bubbles_theme = {
	normal = {
		a = { fg = colors.black, bg = colors.violet },
		b = { fg = colors.white, bg = colors.grey },
		c = { fg = colors.white },
	},

	insert = { a = { fg = colors.black, bg = colors.blue } },
	visual = { a = { fg = colors.black, bg = colors.cyan } },
	replace = { a = { fg = colors.black, bg = colors.red } },

	inactive = {
		a = { fg = colors.white, bg = colors.black },
		b = { fg = colors.white, bg = colors.black },
		c = { fg = colors.white },
	},
}

function M.setup()
	local ok, line = pcall(require, "lualine")
	if not ok then
		return
	end

	local ok, overseer = pcall(require, "overseer")
	if not ok then
		return
	end
	local function parrot_status()
		local status_info = require("parrot.config").get_status_info()
		local status = ""
		if status_info.is_chat then
			status = status_info.prov.chat.name
		else
			status = status_info.prov.command.name
		end
		return string.format("%s(%s)", status, status_info.model)
	end

	line.setup({
		options = {
			theme = bubbles_theme,
			component_separators = "",
			section_separators = { left = "", right = "" },
		},
		sections = {
			lualine_a = { { "mode", separator = { left = "" }, right_padding = 2 } },
			lualine_b = { "filename", "branch" },
			lualine_c = {
				function()
					return require("lsp-progress").progress()
				end,
			},
			lualine_d = {
				{ require("dr-lsp").lspCount },
			},
      lualine_v = { parrot_status },
			lualine_w = { { project_name_display } },
			lualine_x = {
				{
					"overseer",
					label = "", -- Prefix for task counts
					colored = true, -- Color the task icons and counts
					symbols = {
						[overseer.STATUS.FAILURE] = "F:",
						[overseer.STATUS.CANCELED] = "C:",
						[overseer.STATUS.SUCCESS] = "S:",
						[overseer.STATUS.RUNNING] = "R:",
					},
					unique = false, -- Unique-ify non-running task count by name
					name = nil, -- List of task names to search for
					name_not = false, -- When true, invert the name search
					status = nil, -- List of task statuses to display
					status_not = false, -- When true, invert the status search
				},
			},
			lualine_y = { "filetype", "progress" },
			lualine_z = {
				{ "location", separator = { right = "" }, left_padding = 2 },
			},
		},
	})
	vim.api.nvim_create_augroup("lualine_augroup", { clear = true })
	vim.api.nvim_create_autocmd("User", {
		group = "lualine_augroup",
		pattern = "LspProgressStatusUpdated",
		callback = require("lualine").refresh,
	})
end
return M

local M = {}

function M.setup()
	local status_ok, alpha = pcall(require, "alpha")
	if not status_ok then
		return
	end

	local dashboard = require("alpha.themes.dashboard")

	local function header()
		return require("utils.logos")["random"]
	end

	dashboard.section.header.val = header()

	dashboard.section.buttons.val = {
		dashboard.button("e", "  New file", ":ene <BAR> startinsert <CR>"),
		dashboard.button("c", "  Configuration", ":e $MYVIMRC/lua <CR>"),
		dashboard.button("q", "  Quit Neovim", ":qa<CR>"),
	}

	local function footer()
		-- Number of plugins
		local datetime = os.date("%d-%m-%Y %H:%M:%S")

		-- Quote
		local fortune = require("alpha.fortune")
		local quote = table.concat(fortune(), "\n")

		return quote
	end

	dashboard.section.footer.val = footer()

	dashboard.section.footer.opts.hl = "Constant"
	dashboard.section.header.opts.hl = "Include"
	dashboard.section.buttons.opts.hl = "Function"
	dashboard.section.buttons.opts.hl_shortcut = "Type"
	dashboard.opts.opts.noautocmd = true

	alpha.setup(dashboard.opts)
end

return M

-- let $NVIM_TUI_ENABLE_TRUE_COLOR=1
require("keybindings")
vim.loader.enable()
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)
require("plugins")
vim.opt.termguicolors = true
vim.cmd.colorscheme("nightfox")
-- vim.g.python3_host_prog = "/Users/hugo/venvs/.nvim-venv/bin/python"

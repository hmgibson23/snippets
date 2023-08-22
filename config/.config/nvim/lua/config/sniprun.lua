local M = {}

M.setup = function()
	require("sniprun").setup({
		display = { "NvimNotify" },
	})
end

return M

local M = {}

function M.init()
	vim.g.molten_image_provider = "image.nvim"
	vim.g.molten_output_win_max_height = 40
	vim.g.molten_auto_open_output = false
	vim.g.molten_wrap_output = true
	vim.g.molten_virt_text_output = false
	vim.g.molten_virt_lines_off_by_1 = false
	vim.g.molten_use_border_highlights = true
	vim.g.molten_output_win_border = "-"
end

function M.setup()
	-- Undo those config changes when we go back to a markdown or quarto file
	vim.api.nvim_create_autocmd("BufEnter", {
		pattern = { "*.qmd", "*.md", "*.ipynb" },
		callback = function(e)
			if string.match(e.file, ".otter.") then
				return
			end
			if require("molten.status").initialized() == "Molten" then
				vim.fn.MoltenUpdateOption("virt_lines_off_by_1", true)
				vim.fn.MoltenUpdateOption("virt_text_output", true)
			else
				vim.g.molten_virt_lines_off_by_1 = true
				vim.g.molten_virt_text_output = true
			end
		end,
	})
end

return M

-- Function to convert .ipynb to .qmd
local function convert_ipynb_to_qmd()
	local current_file = vim.fn.expand("%:p")
	local converted_file = current_file:gsub(".ipynb$", ".qmd")

	-- Run Quarto convert asynchronously
	vim.fn.jobstart({ "quarto", "convert", current_file, "-o", converted_file }, {
		on_stdout = function(_, data)
			if data then
				vim.api.nvim_out_write(table.concat(data, "\n") .. "\n")
			end
		end,
		on_stderr = function(_, data)
			if data then
				vim.api.nvim_err_write(table.concat(data, "\n") .. "\n")
			end
		end,
		on_exit = function(_, exit_code)
			if exit_code == 0 then
				vim.api.nvim_out_write("Successfully converted .ipynb to .qmd\n")
				-- Change buffer to the new .qmd file
				vim.cmd("edit " .. converted_file)
			else
				vim.api.nvim_err_write("Error converting .ipynb to .qmd\n")
			end
		end,
	})
end

-- Function to convert .qmd to .ipynb
local function convert_qmd_to_ipynb()
	local current_file = vim.fn.expand("%:p")
	local converted_file = current_file:gsub(".qmd$", ".ipynb")

	-- Run Quarto convert asynchronously
	vim.fn.jobstart({ "quarto", "convert", current_file, "-o", converted_file }, {
		on_stdout = function(_, data)
			if data then
				vim.api.nvim_out_write(table.concat(data, "\n") .. "\n")
			end
		end,
		on_stderr = function(_, data)
			if data then
				vim.api.nvim_err_write(table.concat(data, "\n") .. "\n")
			end
		end,
		on_exit = function(_, exit_code)
			if exit_code == 0 then
				vim.api.nvim_out_write("Successfully converted .qmd to .ipynb\n")
				-- Change buffer to the new .ipynb file
				vim.cmd("edit " .. converted_file)
			else
				vim.api.nvim_err_write("Error converting .qmd to .ipynb\n")
			end
		end,
	})
end

-- Autocommand for opening .ipynb files (convert to .qmd)
vim.api.nvim_create_autocmd("BufReadPost", {
	pattern = "*.ipynb",
	callback = function()
		-- Convert .ipynb to .qmd when opening
		convert_ipynb_to_qmd()
	end,
})

-- Autocommand for saving .qmd files (convert to .ipynb)
vim.api.nvim_create_autocmd("BufWritePost", {
	pattern = "*.qmd",
	callback = function()
		-- Convert .qmd back to .ipynb on save
		convert_qmd_to_ipynb()
	end,
})

local M = {}
local whichkey = require("which-key")

function M.setup()
	local ok, harpoon = pcall(require, "harpoon")
	if not ok then
		return
	end

	harpoon:setup()

	local function toggle_telescope(harpoon_files)
		local conf = require("telescope.config").values
		local file_paths = {}
		for _, item in ipairs(harpoon_files.items) do
			table.insert(file_paths, item.value)
		end

		require("telescope.pickers")
			.new({}, {
				prompt_title = "Harpoon",
				finder = require("telescope.finders").new_table({
					results = file_paths,
				}),
				previewer = conf.file_previewer({}),
				sorter = conf.generic_sorter({}),
			})
			:find()
	end

	whichkey.add({
    { "<leader>h", group = "Harpoon"},
    { "<leader>hh", function() toggle_telescope(harpoon:list()) end, desc = "Telescope"},
    { "<leader>ha", function() harpoon:list():add() end, desc = "Add mark"},
  })
end

return M

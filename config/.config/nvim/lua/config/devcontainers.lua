local M = {}

M.setup = function()
	local function run_container(command)
		local handle = io.popen(command)
		local result = handle:read("*a")
		handle:close()
		return result
	end

	require("devcontainer").setup({
		container_runtime = "docker",
		backup_runtime = "podman",
	})
	local devcontainer = require("devcontainer.container")
	local devcontainer_compose = require("devcontainer.compose")
	local toggleterm = require("toggleterm.terminal")
	local function run_docker_compose()
		-- Prompt for the compose file path (optional)
		vim.ui.input(
			{ prompt = "Enter path to docker-compose.yml file (leave blank for default): " },
			function(compose_file)
				-- Use the default compose file if none is provided
				if not compose_file or compose_file == "" then
					compose_file = ".devcontainer/docker-compose.yml" -- Default path
				end

				-- Run docker-compose up using the devcontainer.compose module
				devcontainer_compose.up(compose_file, {
					on_success = function()
						print("Docker Compose started successfully.")
					end,
					on_fail = function(err)
						print("Failed to start Docker Compose.")
						print(err)
					end,
				})
			end
		)
	end

	local function run_container_with_toggleterm()
		vim.ui.input({ prompt = "Enter Docker image name: " }, function(image_name)
			if not image_name or image_name == "" then
				print("No image name provided.")
				return
			end

			vim.ui.input({ prompt = "Enter shell command (default: /bin/bash): " }, function(shell_command)
				if not shell_command or shell_command == "" then
					shell_command = "/bin/bash"
				end

				vim.ui.select({ "Yes", "No" }, {
					prompt = "Do you want to mount the current working directory at /app?",
				}, function(choice)
					local mount_option = ""
					if choice == "Yes" then
						local current_dir = vim.fn.getcwd()
						mount_option = "-v " .. current_dir .. ":/app"
					end

					local command = "podman run -it "
						.. mount_option
						.. " --workdir /app "
						.. image_name
						.. " "
						.. shell_command
					local status = run_container(command)

					if status == "" then -- Podman failed, try Docker
						command = "docker run -it "
							.. mount_option
							.. " --workdir /app "
							.. image_name
							.. " "
							.. shell_command
						status = run_container(command)
						if status == "" then
							print("Failed to run container with both Podman and Docker.")
							return
						end
					end

					local terminal = toggleterm.Terminal:new({
						direction = "horizontal",
						cmd = command,
						display_name = image_name,
					})
					terminal:toggle()
				end)
			end)
		end)
	end
	local whichkey = require("which-key")
	local telescope = require("telescope.builtin")

	local attach_to_container = function()
		local pickers = require("telescope.pickers")
		local actions = require("telescope.actions")
		local action_state = require("telescope.actions.state")
		local toggleterm = require("toggleterm.terminal")
		local finders = require("telescope.finders")

		require("devcontainer.container").container_ls({
			on_success = function(containers)
				local items = {}
				for _, container in ipairs(containers) do
					table.insert(items, {
						display = container.Name, -- Display name of the container
						value = container.Name, -- Use container name for value
					})
				end

				pickers
					.new({}, {
						prompt_title = "Select a Container",
						finder = finders.new_table({
							results = containers,
						}),
						attach_mappings = function(prompt_bufnr, map)
							map("i", "<CR>", function()
								local selection = action_state.get_selected_entry()
								local container_name = selection.value -- Get container name from value

								actions.close(prompt_bufnr)

								-- Prompt for terminal command selection
								vim.ui.select({ "/bin/sh", "/bin/bash", "/bin/zsh" }, {
									prompt = "Select shell command:",
								}, function(shell_command)
									if not shell_command then
										print("No shell command selected.")
										return
									end

									-- Open terminal in container with `toggleterm` using the selected shell command
									toggleterm.Terminal
										:new({
											direction = "horizontal",
											cmd = "docker exec -it " .. container_name .. " " .. shell_command, -- Use container name
											display_name = container_name,
										})
										:toggle()
								end)
							end)
							return true
						end,
					})
					:find()
			end,
		})
	end

	whichkey.add({
		-- DevContainer
		{ "<leader>a", group = "Devcontainers" },
		{ "<leader>as", "<cmd>DevcontainerStart<cr>", desc = "Start" },
		{ "<leader>ax", "<cmd>DevcontainerStop<cr>", desc = "Stop" },
		{ "<leader>ad", "<cmd>DevcontainerDelete<cr>", desc = "Delete" },
		{ "<leader>ai", "<cmd>DevcontainerLogs<cr>", desc = "Logs" },
		{ "<leader>aa", "<cmd>DevcontainerAttach<cr>", desc = "Attach" },
		{ "<leader>ae", "<cmd>DevcontainerExec<cr>", desc = "Execute" },
		{ "<leader>ac", run_docker_compose, desc = "Compose" },
		-- Pass containers to telescope for terminal attach on select
		{ "<leader>al", attach_to_container, desc = "Container terminal" },
		{ "<leader>ar", run_container_with_toggleterm, desc = "Terminal (not Devcontainer)" },
	})
end

return M

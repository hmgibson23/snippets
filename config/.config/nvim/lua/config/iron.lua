local M = {}
local iron = require("iron.core")
local view = require("iron.view")

local function get_db_command(meta)
	local engine = vim.fn.input("Choose Database Engine (postgresql/sqlite): ", "postgresql")
	local db_name = vim.fn.input("Db: ", "", "file")

	if engine == "postgresql" then
		local user = vim.fn.input("User: ", "postgres") -- Default user for PostgreSQL
		local password = vim.fn.input("Password: ", "") -- No default password
		local url = vim.fn.input("URL (default: localhost): ", "localhost") -- Default URL
		-- Construct the command
		return { "psql", "-h", url, "-U", user, db_name }
	elseif engine == "sqlite" then
		return { "sqlite3", db_name }
	else
		local error_message = "Unsupported database engine: " .. engine
		print(error_message) -- Log the error
		vim.notify(error_message, vim.log.levels.ERROR) -- Send a notification for the error
		return nil
	end
end

function M.setup()
	iron.setup({
		config = {
			scratch_repl = false,
			repl_definition = {
				sh = {
					command = { "zsh" },
				},
				python = {
					command = { "ipython" },
					format = require("iron.fts.common").bracketed_paste,
				},
				sql = {
					command = get_db_command,
				},
				java = {
					command = function(meta)
						--        local bufnr = api.nvim_get_current_buf()
						--       local uri = vim.uri_from_bufnr(bufnr)
						--       if vim.startswith(uri, "jdt://") then
						--          options = vim.fn.json_encode({ scope = "runtime" })
						--        else
						--          local err, is_test_file = M.execute_command(is_test_file_cmd, nil, bufnr)
						--          assert(not err, vim.inspect(err))
						--          options = vim.fn.json_encode({
						--            scope = is_test_file and 'test' or 'runtime';
						--          })
						--        end
						--        local cmd = {
						--          command = 'java.project.getClasspaths';
						--          arguments = { uri, options };
						--        }
						return { "jshell" }
					end,
				},
				cpp = {
					command = { "clang-repl" },
				},
			},
			repl_open_cmd = require("iron.view").split.vertical.botright(0.5),
		},

		-- repl_open_cmd = view.right(40),
		keymaps = {
			send_motion = "<space>sc",
			visual_send = "<space>sc",
			send_file = "<space>sf",
			send_line = "<space>sl",
			send_paragraph = "<space>sp",
			send_until_cursor = "<space>su",
			send_mark = "<space>sm",
			mark_motion = "<space>mc",
			mark_visual = "<space>mc",
			remove_mark = "<space>md",
			cr = "<space>s<cr>",
			interrupt = "<space>s<space>",
			exit = "<space>sq",
			clear = "<space>cl",
		},
	})
end

return M

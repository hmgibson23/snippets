local M = {}
local iron = require("iron.core")
local view = require("iron.view")

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
					command = function(meta)
						local db_name = vim.fn.input("Db: ", "", "file")
						return { "sqlite3", db_name }
					end,
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

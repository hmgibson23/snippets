local function get_db_command(meta)
  local engine = vim.fn.input("Choose Database Engine (postgresql/sqlite): ", "postgresql")
  local db_name = vim.fn.input("Db: ", "", "file")

  if engine == "postgresql" then
    local user = vim.fn.input("User: ", "postgres")                   -- Default user for PostgreSQL
    local password = vim.fn.input("Password: ", "")                   -- No default password
    local url = vim.fn.input("URL (default: localhost): ", "localhost") -- Default URL
    -- Construct the command
    return { "psql", "-h", url, "-U", user, db_name }
  elseif engine == "sqlite" then
    return { "sqlite3", db_name }
  else
    local error_message = "Unsupported database engine: " .. engine
    print(error_message)                          -- Log the error
    vim.notify(error_message, vim.log.levels.ERROR) -- Send a notification for the error
    return nil
  end
end

return {
  "hkupty/iron.nvim",
  cmd = { "IronRepl", "IronFocus", "IronHide", "IronRestart" },
  keys = {
    { "<leader>ii", "<cmd>IronRepl<cr>", desc = "Open REPL" },
    { "<leader>if", "<cmd>IronFocus<cr>", desc = "Focus REPL" },
    { "<leader>ih", "<cmd>IronHide<cr>", desc = "Hide REPL" },
    { "<leader>ir", "<cmd>IronRestart<cr>", desc = "Restart REPL" },
    { "<leader>iF", function() require("iron.core").send_file() end, desc = "Send File" },
    { "<leader>il", function() require("iron.core").send_line("r") end, desc = "Send Line" },
  },
  config = function()
    local iron = require("iron")
    local icons = require("config.icons")
    local whichkey = require("which-key")
    whichkey.add({

      { "<leader>i",  group = "Iron",                              icon = icons.kind.Package },
      {
        "<leader>ii",
        "<cmd>IronRepl<cr>",
        desc = "Open REPL",
        icon = icons.misc.Terminal,
      },
      {
        "<leader>if",
        "<cmd>IronFocus<cr>",
        desc = "Focus REPL",
        icon = icons.kind.Search,
      },
      {
        "<leader>ih",
        "<cmd>IronHide<cr>",
        desc = "Hide REPL",
        icon = icons.misc.Secret,
      },
      { "<leader>ir", "<cmd>IronRestart<cr>",                      desc = "Restart REPL" },
      { "<leader>iF", ":lua require('iron.core').send_file()<cr>", desc = "Send File" },
      {
        "<leader>il",
        ":lua require('iron.core').send_line('r')<cr>",
        desc = "Send Line",
        icon = icons.kind.File,
      },
    })
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
          quarto = {
            command = { "ipython" },
            format = require("iron.fts.common").bracketed_paste,
          },
          sql = {
            command = get_db_command,
          },
          java = {
            command = function(meta)
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
  end,
}

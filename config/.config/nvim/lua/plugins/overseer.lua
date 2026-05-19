return {
  "stevearc/overseer.nvim",
  dependencies = {
    -- "pianocomposer321/officer.nvim",
    "franco-ruggeri/overseer-extra.nvim",
  },
  cmd = {
    "OverseerToggle",
    "OverseerOpen",
    "OverseerRun",
    "OverseerBuild",
    "OverseerClose",
    "OverseerLoadBundle",
    "OverseerSaveBundle",
    "OverseerDeleteBundle",
    "OverseerRunCmd",
    "OverseerQuickAction",
    "OverseerTaskAction",
  },
  config = function()
    local overseer = require("overseer")
    local whichkey = require("which-key")
    local icons = require("config.icons")
    whichkey.add({
      { "<leader>o", group = "[O]verseer" },
      { "<leader>ob", "<cmd>OverseerBuild<cr>", desc = "Build", icon = "" },
      { "<leader>or", "<cmd>OverseerRun<cr>", desc = "Run", icon = "" },
      { "<leader>ot", "<cmd>OverseerToggle<cr>", desc = "Toggle" },
      { "<leader>oc", "<cmd>OverseerRunCmd<cr>", desc = "Run Command", icon = icons.misc.Terminal },
    })

    vim.api.nvim_create_user_command("OverseerRestartLast", function()
      local tasks = overseer.list_tasks({ recent_first = true })
      if vim.tbl_isempty(tasks) then
        vim.notify("No tasks found", vim.log.levels.WARN)
      else
        overseer.run_action(tasks[1], "restart")
      end
    end, {})

    vim.api.nvim_create_user_command("OGrep", function(params)
      -- Insert args at the '$*' in the grepprg
      local cmd, num_subs = vim.o.grepprg:gsub("%$%*", params.args)
      if num_subs == 0 then
        cmd = cmd .. " " .. params.args
      end
      local task = overseer.new_task({
        cmd = vim.fn.expandcmd(cmd),
        components = {
          {
            "on_output_quickfix",
            errorformat = vim.o.grepformat,
            open = not params.bang,
            open_height = 8,
            items_only = true,
          },
          -- We don't care to keep this around as long as most tasks
          { "on_complete_dispose", timeout = 30 },
          "default",
        },
      })
      task:start()
    end, { nargs = "*", bang = true, complete = "file" })

    overseer.setup()
  end,
}

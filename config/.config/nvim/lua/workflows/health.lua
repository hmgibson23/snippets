local select = require("workflows.select")

local M = {}

local function term(cmd)
  vim.cmd("botright split")
  vim.cmd("terminal " .. cmd)
end

local checks = {
  {
    label = "Config: headless load",
    command = "nvim --headless +'lua print(\"config loaded\")' +qa",
  },
  {
    label = "Config: Lua syntax",
    command = "find lua -name '*.lua' -print0 | xargs -0 -n1 luac -p",
  },
  {
    label = "Config: git diff check",
    command = "git diff --check",
  },
  {
    label = "Lazy: health",
    nvim = function()
      vim.cmd("checkhealth lazy")
    end,
  },
  {
    label = "Lazy: profile",
    nvim = function()
      vim.cmd("Lazy profile")
    end,
  },
  {
    label = "Lazy: sync",
    nvim = function()
      vim.cmd("Lazy sync")
    end,
  },
}

function M.run_check(item)
  if item.nvim then
    item.nvim()
  else
    term(item.command)
  end
end

function M.palette()
  select.run("Config health", vim.tbl_map(function(item)
    return {
      label = item.label,
      action = function()
        M.run_check(item)
      end,
    }
  end, checks))
end

function M.setup()
  vim.api.nvim_create_user_command("ConfigHealth", M.palette, { desc = "Config maintenance palette" })
  vim.api.nvim_create_user_command("ConfigProfile", function() vim.cmd("Lazy profile") end, { desc = "Open Lazy profile" })
  vim.api.nvim_create_user_command("ConfigReload", function()
    dofile(vim.env.MYVIMRC)
    vim.notify("Reloaded " .. vim.env.MYVIMRC)
  end, { desc = "Reload init.lua" })

  vim.keymap.set("n", "<leader>ch", M.palette, { desc = "Config health" })
  vim.keymap.set("n", "<leader>cp", "<cmd>ConfigProfile<cr>", { desc = "Config profile" })
  vim.keymap.set("n", "<leader>cr", "<cmd>ConfigReload<cr>", { desc = "Config reload" })
end

return M

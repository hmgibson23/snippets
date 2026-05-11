local project = require("workflows.project")

local M = {}

local function safe_name(value)
  return value:gsub("[^A-Za-z0-9_.-]", "_")
end

function M.name()
  local info = project.info()
  local branch = info.branch and ("__" .. safe_name(info.branch)) or ""
  return safe_name(info.name) .. branch
end

function M.write()
  local ok, sessions = pcall(require, "mini.sessions")
  if ok then
    sessions.write(M.name())
  else
    vim.cmd("mksession! " .. vim.fn.stdpath("data") .. "/session/" .. M.name() .. ".vim")
  end
end

function M.read()
  local ok, sessions = pcall(require, "mini.sessions")
  if ok then
    sessions.read(M.name())
  else
    vim.cmd("source " .. vim.fn.stdpath("data") .. "/session/" .. M.name() .. ".vim")
  end
end

function M.select()
  local ok, sessions = pcall(require, "mini.sessions")
  if ok then
    sessions.select()
  else
    vim.cmd("browse oldfiles")
  end
end

function M.setup()
  vim.api.nvim_create_user_command("SessionSaveProject", M.write, { desc = "Save project/branch session" })
  vim.api.nvim_create_user_command("SessionLoadProject", M.read, { desc = "Load project/branch session" })
  vim.api.nvim_create_user_command("SessionSelect", M.select, { desc = "Select a MiniSessions session" })

  vim.keymap.set("n", "<leader>ps", M.write, { desc = "Save project session" })
  vim.keymap.set("n", "<leader>pl", M.read, { desc = "Load project session" })
  vim.keymap.set("n", "<leader>pS", M.select, { desc = "Select session" })
end

return M

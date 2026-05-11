local ai = require("workflows.ai")
local code = require("workflows.code")
local diagnostics = require("workflows.diagnostics")
local health = require("workflows.health")
local project = require("workflows.project")
local sessions = require("workflows.sessions")
local tasks = require("workflows.tasks")
local select = require("workflows.select")

local M = {}

function M.items()
  return {
    { label = "AI actions", action = ai.palette },
    { label = "Code actions", action = code.palette },
    { label = "Tasks/tests/debug", action = tasks.palette },
    { label = "Diagnostics", action = diagnostics.palette },
    { label = "Project info", action = function() vim.notify(project.describe()) end },
    { label = "Open project root", action = project.open_root },
    { label = "Edit Neovim config", action = project.open_config },
    { label = "Save project session", action = sessions.write },
    { label = "Load project session", action = sessions.read },
    { label = "Config health", action = health.palette },
    {
      label = "Git status",
      action = function()
        if package.loaded["snacks"] then
          Snacks.picker.git_status()
        else
          vim.cmd("Neogit")
        end
      end,
    },
    {
      label = "Find files",
      action = function()
        if package.loaded["snacks"] then
          Snacks.picker.files()
        else
          vim.cmd("edit .")
        end
      end,
    },
    {
      label = "Grep project",
      action = function()
        if package.loaded["snacks"] then
          Snacks.picker.grep()
        else
          vim.cmd("grep ")
        end
      end,
    },
  }
end

function M.open()
  select.run("Workspace palette", M.items())
end

return M

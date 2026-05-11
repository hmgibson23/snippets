local project = require("workflows.project")
local select = require("workflows.select")

local M = {}

local function shell(cmd)
  if vim.fn.exists(":OverseerRunCmd") == 2 then
    vim.api.nvim_cmd({ cmd = "OverseerRunCmd", args = { cmd } }, {})
  else
    vim.cmd("split | terminal " .. cmd)
  end
end

local function neotest_run(opts)
  local ok, neotest = pcall(require, "neotest")
  if not ok then
    vim.notify("neotest is not available", vim.log.levels.WARN)
    return
  end
  neotest.run.run(opts)
end

function M.run_nearest()
  neotest_run()
end

function M.run_file()
  neotest_run(vim.fn.expand("%"))
end

function M.run_suite()
  neotest_run({ suite = true })
end

function M.debug_nearest()
  neotest_run({ strategy = "dap" })
end

function M.debug_file()
  neotest_run({ vim.fn.expand("%"), strategy = "dap" })
end

local function run_project_command(kind)
  local command = project.command(kind)
  if command then
    shell(command)
  else
    vim.cmd("OverseerRun")
  end
end

function M.run_project_default()
  run_project_command("test")
end

function M.run_project()
  run_project_command("run")
end

function M.build_project()
  run_project_command("build")
end

function M.format_project()
  run_project_command("format")
end

function M.actions()
  return {
    { label = "Test nearest", action = M.run_nearest },
    { label = "Test file", action = M.run_file },
    { label = "Test suite", action = M.run_suite },
    { label = "Debug nearest test", action = M.debug_nearest },
    { label = "Debug file tests", action = M.debug_file },
    { label = "Run project tests", action = M.run_project_default },
    { label = "Run project app", action = M.run_project },
    { label = "Build/render project", action = M.build_project },
    { label = "Format project", action = M.format_project },
    { label = "Overseer toggle", action = function() vim.cmd("OverseerToggle") end },
    { label = "Overseer run", action = function() vim.cmd("OverseerRun") end },
    { label = "Neotest summary", action = function() require("neotest").summary.toggle() end },
    { label = "Neotest output", action = function() require("neotest").output.open({ enter = true }) end },
  }
end

function M.palette()
  select.run("Tasks and tests", M.actions())
end

function M.setup()
  vim.api.nvim_create_user_command("TaskPalette", M.palette, { desc = "Task/test/debug palette" })
  vim.api.nvim_create_user_command("ProjectTest", M.run_project_default, { desc = "Run default project test command" })
  vim.api.nvim_create_user_command("ProjectRun", M.run_project, { desc = "Run default project app command" })
  vim.api.nvim_create_user_command("ProjectBuild", M.build_project, { desc = "Run default project build/render command" })
  vim.api.nvim_create_user_command("ProjectFormat", M.format_project, { desc = "Run default project format command" })

  vim.keymap.set("n", "<leader>kn", M.run_nearest, { desc = "Test nearest" })
  vim.keymap.set("n", "<leader>kf", M.run_file, { desc = "Test file" })
  vim.keymap.set("n", "<leader>ka", M.run_suite, { desc = "Test suite" })
  vim.keymap.set("n", "<leader>kd", M.debug_nearest, { desc = "Debug nearest test" })
  vim.keymap.set("n", "<leader>kD", M.debug_file, { desc = "Debug file tests" })
  vim.keymap.set("n", "<leader>kr", M.run_project_default, { desc = "Run project tests" })
  vim.keymap.set("n", "<leader>kR", M.run_project, { desc = "Run project app" })
  vim.keymap.set("n", "<leader>kb", M.build_project, { desc = "Build/render project" })
  vim.keymap.set("n", "<leader>kF", M.format_project, { desc = "Format project" })
  vim.keymap.set("n", "<leader>kp", M.palette, { desc = "Task palette" })
end

return M

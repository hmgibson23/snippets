local project = require("workflows.project")
local select = require("workflows.select")

local M = {}

local function has_module(name)
  return pcall(require, name)
end

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

function M.run_project_default()
  local info = project.info()
  if vim.tbl_contains(info.types, "python") then
    shell(vim.uv.fs_stat(info.root .. "/uv.lock") and "uv run pytest" or "pytest")
  elseif vim.tbl_contains(info.types, "node") then
    shell("npm test")
  elseif vim.tbl_contains(info.types, "rust") then
    shell("cargo test")
  elseif vim.tbl_contains(info.types, "go") then
    shell("go test ./...")
  else
    vim.cmd("OverseerRun")
  end
end

function M.actions()
  return {
    { label = "Test nearest", action = M.run_nearest },
    { label = "Test file", action = M.run_file },
    { label = "Test suite", action = M.run_suite },
    { label = "Debug nearest test", action = M.debug_nearest },
    { label = "Debug file tests", action = M.debug_file },
    { label = "Run project default", action = M.run_project_default },
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

  vim.keymap.set("n", "<leader>kn", M.run_nearest, { desc = "Test nearest" })
  vim.keymap.set("n", "<leader>kf", M.run_file, { desc = "Test file" })
  vim.keymap.set("n", "<leader>ka", M.run_suite, { desc = "Test suite" })
  vim.keymap.set("n", "<leader>kd", M.debug_nearest, { desc = "Debug nearest test" })
  vim.keymap.set("n", "<leader>kD", M.debug_file, { desc = "Debug file tests" })
  vim.keymap.set("n", "<leader>kr", M.run_project_default, { desc = "Run project default" })
  vim.keymap.set("n", "<leader>kp", M.palette, { desc = "Task palette" })
end

return M

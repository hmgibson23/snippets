local M = {}

local function executable(command)
  return vim.fn.executable(command) == 1
end

local function shell(cmd)
  if vim.fn.exists(":OverseerRunCmd") == 2 then
    vim.api.nvim_cmd({ cmd = "OverseerRunCmd", args = { cmd } }, {})
  else
    vim.cmd("split | terminal " .. cmd)
  end
end

function M.root(bufnr)
  bufnr = bufnr or 0
  local name = vim.api.nvim_buf_get_name(bufnr)
  local start = name ~= "" and vim.fs.dirname(name) or vim.uv.cwd()
  return vim.fs.root(start, { "*.slnx", "*.sln", "*.csproj", "omnisharp.json", "function.json", ".git" }) or vim.uv.cwd()
end

function M.project_files(root)
  root = root or M.root()
  local files = {}
  for _, pattern in ipairs({ "*.slnx", "*.sln", "**/*.csproj" }) do
    for _, file in ipairs(vim.fn.glob(root .. "/" .. pattern, false, true)) do
      table.insert(files, file)
    end
  end
  return files
end

function M.choose_target(targets)
  if not targets or #targets == 0 then
    return nil
  end

  local sorted = vim.deepcopy(targets)
  table.sort(sorted)

  for _, extension in ipairs({ "%.slnx$", "%.sln$", "%.slnf$", "%.csproj$" }) do
    for _, target in ipairs(sorted) do
      if target:match(extension) then
        return target
      end
    end
  end

  return sorted[1]
end

function M.roslyn_executable()
  local mason_bin = vim.fs.joinpath(vim.fn.stdpath("data"), "mason", "bin", vim.fn.has("win32") == 1 and "roslyn.cmd" or "roslyn")
  if vim.fn.executable(mason_bin) == 1 then
    return mason_bin
  end
  local roslyn = vim.fn.exepath("roslyn")
  if roslyn ~= "" then
    return roslyn
  end
  local language_server = vim.fn.exepath("Microsoft.CodeAnalysis.LanguageServer")
  if language_server ~= "" then
    return language_server
  end
  return nil
end

function M.command(kind, root)
  root = root or M.root()
  local commands = {
    build = "dotnet build",
    test = "dotnet test",
    restore = "dotnet restore",
    clean = "dotnet clean",
    watch_test = "dotnet watch test",
  }

  if kind == "format" then
    if executable("csharpier") then
      return "csharpier ."
    end
    return "dotnet format"
  end

  return commands[kind]
end

function M.run(kind)
  local cmd = M.command(kind)
  if not cmd then
    vim.notify("Unknown C# command: " .. tostring(kind), vim.log.levels.WARN)
    return
  end
  shell(cmd)
end

function M.info()
  local root = M.root()
  local files = M.project_files(root)
  local roslyn = M.roslyn_executable()
  local dotnet_version = vim.fn.executable("dotnet") == 1 and vim.fn.systemlist("dotnet --version")[1] or nil

  local lines = {
    "C# status",
    "=========",
    "root: " .. root,
    "project files: " .. (#files > 0 and table.concat(files, ", ") or "none"),
    "dotnet: " .. (dotnet_version or "missing"),
    "roslyn: " .. (roslyn or "not found (run :MasonInstall roslyn after registry update)"),
    "csharpier: " .. (vim.fn.executable("csharpier") == 1 and "ok" or "missing (format falls back to dotnet format)"),
    "active Roslyn clients: " .. tostring(#vim.lsp.get_clients({ name = "roslyn" })),
  }

  vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO, { title = "C#" })
end

function M.actions()
  return {
    { label = "Build", action = function() M.run("build") end },
    { label = "Test", action = function() M.run("test") end },
    { label = "Watch tests", action = function() M.run("watch_test") end },
    { label = "Restore", action = function() M.run("restore") end },
    { label = "Clean", action = function() M.run("clean") end },
    { label = "Format", action = function() M.run("format") end },
    { label = "Select Roslyn target", action = function() vim.cmd("Roslyn target") end },
    { label = "Info", action = M.info },
  }
end

function M.palette()
  local ok, select = pcall(require, "workflows.select")
  if ok then
    select.run("C#", M.actions())
    return
  end

  vim.ui.select(M.actions(), {
    prompt = "C# action",
    format_item = function(item)
      return item.label
    end,
  }, function(choice)
    if choice then
      choice.action()
    end
  end)
end

function M.setup()
  vim.api.nvim_create_user_command("CSharpBuild", function() M.run("build") end, { desc = "Build C# project" })
  vim.api.nvim_create_user_command("CSharpTest", function() M.run("test") end, { desc = "Test C# project" })
  vim.api.nvim_create_user_command("CSharpWatchTest", function() M.run("watch_test") end, { desc = "Watch C# tests" })
  vim.api.nvim_create_user_command("CSharpRestore", function() M.run("restore") end, { desc = "Restore C# project" })
  vim.api.nvim_create_user_command("CSharpClean", function() M.run("clean") end, { desc = "Clean C# project" })
  vim.api.nvim_create_user_command("CSharpFormat", function() M.run("format") end, { desc = "Format C# project" })
  vim.api.nvim_create_user_command("CSharpInfo", M.info, { desc = "Show C# tooling information" })
  vim.api.nvim_create_user_command("CSharpTarget", function() vim.cmd("Roslyn target") end, { desc = "Select Roslyn solution/project target" })
  vim.api.nvim_create_user_command("CSharpPalette", M.palette, { desc = "C# action palette" })
end

return M

local M = {}

local function csharp_command(kind)
  local ok, csharp = pcall(require, "csharp")
  if ok then
    return csharp.command(kind)
  end
  return nil
end

local markers = {
  python = { "pyproject.toml", "uv.lock", "requirements.txt", "setup.py", "pytest.ini" },
  node = { "package.json", "pnpm-lock.yaml", "yarn.lock", "package-lock.json" },
  rust = { "Cargo.toml" },
  go = { "go.mod" },
  csharp = { "*.sln", "*.csproj" },
  quarto = { "_quarto.yml", "*.qmd" },
  godot = { "project.godot" },
  nix = { "flake.nix", "configuration.nix" },
}

local function has_glob(root, pattern)
  if pattern:find("*", 1, true) then
    return #vim.fn.glob(root .. "/" .. pattern, false, true) > 0
  end
  return vim.uv.fs_stat(root .. "/" .. pattern) ~= nil
end

function M.root(bufnr)
  bufnr = bufnr or 0
  local name = vim.api.nvim_buf_get_name(bufnr)
  local start = name ~= "" and vim.fs.dirname(name) or vim.uv.cwd()
  local root = vim.fs.root(start, { ".git", "pyproject.toml", "package.json", "Cargo.toml", "go.mod", "*.slnx", "*.sln", "*.csproj", "project.godot", "_quarto.yml" })
  return root or vim.uv.cwd()
end

function M.detect(root)
  root = root or M.root()
  local found = {}
  for kind, kind_markers in pairs(markers) do
    for _, marker in ipairs(kind_markers) do
      if has_glob(root, marker) then
        table.insert(found, kind)
        break
      end
    end
  end
  table.sort(found)
  return found
end

local function file_contains(path, pattern)
  if not vim.uv.fs_stat(path) then
    return false
  end
  local ok, lines = pcall(vim.fn.readfile, path, "", 200)
  if not ok then
    return false
  end
  return table.concat(lines, "\n"):find(pattern) ~= nil
end

local function glob_contains(root, glob, pattern)
  for _, file in ipairs(vim.fn.glob(root .. "/" .. glob, false, true)) do
    if file_contains(file, pattern) then
      return true
    end
  end
  return false
end

function M.features(root)
  root = root or M.root()
  local features = {}

  if vim.uv.fs_stat(root .. "/uv.lock") or file_contains(root .. "/pyproject.toml", "%[tool%.uv%]") then
    table.insert(features, "uv")
  end
  if vim.uv.fs_stat(root .. "/pytest.ini") or file_contains(root .. "/pyproject.toml", "%[tool%.pytest") then
    table.insert(features, "pytest")
  end
  if glob_contains(root, "**/*.py", "from fastapi import") or glob_contains(root, "**/*.py", "FastAPI%(") then
    table.insert(features, "fastapi")
  end
  if vim.uv.fs_stat(root .. "/manage.py") or file_contains(root .. "/pyproject.toml", "django") then
    table.insert(features, "django")
  end

  table.sort(features)
  return features
end

function M.info(bufnr)
  local root = M.root(bufnr)
  local branch = vim.fn.systemlist("git -C " .. vim.fn.shellescape(root) .. " branch --show-current 2>/dev/null")[1]
  return {
    root = root,
    name = vim.fs.basename(root),
    branch = branch ~= "" and branch or nil,
    types = M.detect(root),
    features = M.features(root),
  }
end

function M.has(kind, bufnr)
  return vim.tbl_contains(M.info(bufnr).types, kind)
end

function M.describe(bufnr)
  local info = M.info(bufnr)
  local types = #info.types > 0 and table.concat(info.types, ", ") or "unknown"
  local features = #info.features > 0 and table.concat(info.features, ", ") or "none"
  local branch = info.branch and (" @ " .. info.branch) or ""
  return string.format("%s%s\n%s\nTypes: %s\nFeatures: %s", info.name, branch, info.root, types, features)
end

function M.command(kind, bufnr)
  local info = M.info(bufnr)
  local types = info.types
  local features = info.features

  if kind == "test" then
    if vim.tbl_contains(types, "python") then
      if vim.tbl_contains(features, "django") then
        return vim.tbl_contains(features, "uv") and "uv run python manage.py test" or "python manage.py test"
      end
      return vim.tbl_contains(features, "uv") and "uv run pytest" or "pytest"
    elseif vim.tbl_contains(types, "node") then
      return "npm test"
    elseif vim.tbl_contains(types, "rust") then
      return "cargo test"
    elseif vim.tbl_contains(types, "go") then
      return "go test ./..."
    elseif vim.tbl_contains(types, "csharp") then
      return csharp_command("test") or "dotnet test"
    end
  elseif kind == "run" then
    if vim.tbl_contains(features, "fastapi") then
      return vim.tbl_contains(features, "uv") and "uv run uvicorn main:app --reload" or "uvicorn main:app --reload"
    elseif vim.tbl_contains(features, "django") then
      return vim.tbl_contains(features, "uv") and "uv run python manage.py runserver" or "python manage.py runserver"
    elseif vim.tbl_contains(types, "node") then
      return "npm run dev"
    elseif vim.tbl_contains(types, "rust") then
      return "cargo run"
    elseif vim.tbl_contains(types, "go") then
      return "go run ."
    elseif vim.tbl_contains(types, "godot") then
      return "godot --path ."
    end
  elseif kind == "build" then
    if vim.tbl_contains(types, "node") then
      return "npm run build"
    elseif vim.tbl_contains(types, "rust") then
      return "cargo build"
    elseif vim.tbl_contains(types, "go") then
      return "go build ./..."
    elseif vim.tbl_contains(types, "csharp") then
      return csharp_command("build") or "dotnet build"
    elseif vim.tbl_contains(types, "quarto") then
      return "quarto render"
    end
  elseif kind == "format" then
    if vim.tbl_contains(types, "python") then
      return vim.tbl_contains(features, "uv") and "uv run ruff format ." or "ruff format ."
    elseif vim.tbl_contains(types, "rust") then
      return "cargo fmt"
    elseif vim.tbl_contains(types, "go") then
      return "gofmt -w ."
    elseif vim.tbl_contains(types, "csharp") then
      return csharp_command("format") or "dotnet format"
    elseif vim.tbl_contains(types, "quarto") then
      return "quarto render --to markdown"
    end
  end

  return nil
end

function M.open_config()
  local config = vim.fn.stdpath("config")
  if package.loaded["snacks"] then
    Snacks.picker.files({ cwd = config })
  else
    vim.cmd.edit(config .. "/init.lua")
  end
end

function M.open_root()
  vim.cmd.edit(M.root())
end

function M.setup()
  vim.api.nvim_create_user_command("ProjectInfo", function()
    vim.notify(M.describe(), vim.log.levels.INFO, { title = "Project" })
  end, { desc = "Show detected project info" })

  vim.api.nvim_create_user_command("ProjectRoot", function()
    vim.fn.setreg("+", M.root())
    vim.notify("Copied project root: " .. M.root())
  end, { desc = "Copy project root to clipboard" })

  vim.api.nvim_create_user_command("ConfigEdit", function()
    M.open_config()
  end, { desc = "Open Neovim config picker" })
end

return M

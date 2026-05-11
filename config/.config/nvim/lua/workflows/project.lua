local M = {}

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
  local root = vim.fs.root(start, { ".git", "pyproject.toml", "package.json", "Cargo.toml", "go.mod", "project.godot", "_quarto.yml" })
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

function M.info(bufnr)
  local root = M.root(bufnr)
  local branch = vim.fn.systemlist({ "git", "-C", root, "branch", "--show-current" })[1]
  return {
    root = root,
    name = vim.fs.basename(root),
    branch = branch ~= "" and branch or nil,
    types = M.detect(root),
  }
end

function M.has(kind, bufnr)
  return vim.tbl_contains(M.info(bufnr).types, kind)
end

function M.describe(bufnr)
  local info = M.info(bufnr)
  local types = #info.types > 0 and table.concat(info.types, ", ") or "unknown"
  local branch = info.branch and (" @ " .. info.branch) or ""
  return string.format("%s%s\n%s\nTypes: %s", info.name, branch, info.root, types)
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

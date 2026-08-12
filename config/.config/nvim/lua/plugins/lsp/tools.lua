local M = {}

local function has_filetype(spec, filetype)
  return not spec.filetypes or vim.tbl_contains(spec.filetypes, filetype)
end

-- Names here are Mason package/adapter identifiers, grouped by how they are used.
M.formatters = {
  {
    builtin = "prettierd",
    command = "prettierd",
    mason = "prettierd",
    filetypes = { "css", "html", "javascript", "javascriptreact", "json", "jsonc", "markdown", "scss", "typescript", "typescriptreact", "vue", "yaml" },
  },
  { builtin = "shfmt", command = "shfmt", mason = "shfmt", filetypes = { "bash", "sh", "zsh" } },
  { builtin = "black", command = "black", mason = "black", filetypes = { "python" } },
  { builtin = "isort", command = "isort", mason = "isort", filetypes = { "python" } },
  { builtin = "stylua", command = "stylua", mason = "stylua", filetypes = { "lua", "teal", "tl", "script" } },
  { builtin = "google_java_format", command = "google-java-format", mason = "google-java-format", filetypes = { "java" } },
  { builtin = "clang_format", command = "clang-format", mason = "clang-format", filetypes = { "c", "cpp", "cuda", "objc", "objcpp" } },
  { builtin = "csharpier", command = "csharpier", mason = "csharpier", filetypes = { "cs" } },
  { builtin = "terraform_fmt", command = "terraform", mason = "terraform", filetypes = { "terraform", "hcl" } },
}

M.linters = {
  { builtin = "checkmake", command = "checkmake", mason = "checkmake", filetypes = { "make" } },
  { builtin = "hadolint", command = "hadolint", mason = "hadolint", filetypes = { "dockerfile" } },
  { builtin = "markdownlint_cli2", command = "markdownlint-cli2", mason = "markdownlint-cli2", filetypes = { "markdown", "markdown.mdx", "pandoc" } },
  { builtin = "pylint", command = "pylint", mason = "pylint", filetypes = { "python" } },
  { builtin = "write_good", command = "write-good", mason = "write-good", filetypes = { "markdown", "markdown.mdx", "pandoc", "text" } },
  { builtin = "cmake_lint", command = "cmake-lint", mason = false, filetypes = { "cmake" } },
  { builtin = "cppcheck", command = "cppcheck", mason = false, filetypes = { "c", "cpp" } },
  { builtin = "zsh", command = "zsh", mason = false, filetypes = { "zsh" } },
  { builtin = "swiftlint", command = "swiftlint", mason = "swiftlint", filetypes = { "swift" } },
  { builtin = "terraform_validate", command = "terraform", mason = "terraform", filetypes = { "terraform", "hcl" } },
  { builtin = "tfsec", command = "tfsec", mason = "tfsec", filetypes = { "terraform", "hcl" } },
}

M.code_actions = {
  { builtin = "gitsigns" },
  { builtin = "gitrebase", command = "git" },
  { builtin = "refactoring" },
  { builtin = "proselint", command = "proselint", mason = "proselint", filetypes = { "markdown", "markdown.mdx", "pandoc", "text" } },
}

M.hovers = {
  { builtin = "dictionary" },
}

M.dap_adapters = {
  { name = "python", mason = "debugpy", filetypes = { "python" } },
  { name = "cppdbg", mason = "cpptools", filetypes = { "c", "cpp" } },
  { name = "delve", mason = "delve", filetypes = { "go" } },
  { name = "js", mason = "js-debug-adapter", filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact" } },
  { name = "coreclr", mason = "netcoredbg", filetypes = { "cs", "fsharp" } },
}

function M.for_filetype(filetype)
  local specs = {}
  local seen = {}

  local function add(spec)
    if spec.mason and not seen[spec.mason] and has_filetype(spec, filetype) then
      seen[spec.mason] = true
      table.insert(specs, spec)
    end
  end

  for _, group in ipairs({ M.formatters, M.linters, M.code_actions }) do
    for _, spec in ipairs(group) do
      add(spec)
    end
  end

  return specs
end

function M.dap_for_filetype(filetype)
  local adapters = {}
  for _, adapter in ipairs(M.dap_adapters) do
    if vim.tbl_contains(adapter.filetypes, filetype) then
      table.insert(adapters, adapter)
    end
  end
  return adapters
end

function M.mason_packages()
  local packages = {}
  local seen = {}

  local function add(package)
    if package and package ~= false and not seen[package] then
      seen[package] = true
      table.insert(packages, package)
    end
  end

  for _, group in ipairs({ M.formatters, M.linters, M.code_actions }) do
    for _, tool in ipairs(group) do
      add(tool.mason)
    end
  end

  return packages
end

return M

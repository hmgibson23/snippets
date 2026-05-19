local M = {}

-- Names here are Mason package/adapter identifiers, grouped by how they are used.
M.formatters = {
  { builtin = "prettierd", command = "prettierd", mason = "prettierd" },
  { builtin = "shfmt", command = "shfmt", mason = "shfmt" },
  { builtin = "black", command = "black", mason = "black" },
  { builtin = "isort", command = "isort", mason = "isort" },
  { builtin = "stylua", command = "stylua", mason = "stylua", opts = { filetypes = { "lua", "teal", "tl", "script" } } },
  { builtin = "google_java_format", command = "google-java-format", mason = "google-java-format" },
  { builtin = "clang_format", command = "clang-format", mason = "clang-format" },
  { builtin = "csharpier", command = "csharpier", mason = "csharpier" },
  { builtin = "terraform_fmt", command = "terraform", mason = "terraform" },
}

M.linters = {
  { builtin = "checkmake", command = "checkmake", mason = "checkmake" },
  { builtin = "hadolint", command = "hadolint", mason = "hadolint" },
  { builtin = "markdownlint_cli2", command = "markdownlint-cli2", mason = "markdownlint-cli2" },
  { builtin = "pylint", command = "pylint", mason = "pylint" },
  { builtin = "write_good", command = "write-good", mason = "write-good" },
  { builtin = "cmake_lint", command = "cmake-lint", mason = false },
  { builtin = "cppcheck", command = "cppcheck", mason = false },
  { builtin = "zsh", command = "zsh", mason = false },
  { builtin = "swiftlint", command = "swiftlint", mason = "swiftlint" },
  { builtin = "terraform_validate", command = "terraform", mason = "terraform" },
  { builtin = "tfsec", command = "tfsec", mason = "tfsec" },
}

M.code_actions = {
  { builtin = "gitsigns" },
  { builtin = "gitrebase", command = "git" },
  { builtin = "refactoring" },
  { builtin = "proselint", command = "proselint", mason = "proselint" },
}

M.hovers = {
  { builtin = "dictionary" },
}

M.dap_adapters = {
  "python",
  "cppdbg",
  "delve",
  "js",
  "coreclr",
}

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

return {
  "Civitasv/cmake-tools.nvim",
  opts = {
    cmake_build_directory = "build",
    cmake_generate_options = { "-D", "CMAKE_EXPORT_COMPILE_COMMANDS=1" },
  },
  keys = {
    { "<leader>[cs",  "<cmd>CMakeSettings<cr>",         desc = "Cmake Settings" },
    { "<leader>[cr",  "<cmd>CMakeRun<cr>",              desc = "Cmake Run" },
    { "<leader>[cfr", "<cmd>CMakeRunCurrentFile<cr>",   desc = "Cmake Run Current File" },
    { "<leader>[cfb", "<cmd>CMakeBuildCurrentFile<cr>", desc = "Cmake Build Current File" },
    { "<leader>[cg",  "<cmd>CMakeGenerate<cr>",         desc = "Cmake Generate" },
  },
}

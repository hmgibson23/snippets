return {
  {
    "seblyng/roslyn.nvim",
    ft = { "cs", "razor" },
    cmd = { "Roslyn", "CSharpPalette", "CSharpInfo", "CSharpTarget" },
    opts = {
      filewatching = "roslyn",
      broad_search = true,
      choose_target = function(targets)
        return require("csharp").choose_target(targets)
      end,
      lock_target = true,
      silent = false,
    },
    config = function(_, opts)
      vim.lsp.config("roslyn", {
        settings = {
          ["csharp|inlay_hints"] = {
            csharp_enable_inlay_hints_for_implicit_object_creation = true,
            csharp_enable_inlay_hints_for_implicit_variable_types = true,
            csharp_enable_inlay_hints_for_lambda_parameter_types = true,
            csharp_enable_inlay_hints_for_types = true,
            dotnet_enable_inlay_hints_for_indexer_parameters = true,
            dotnet_enable_inlay_hints_for_literal_parameters = true,
            dotnet_enable_inlay_hints_for_object_creation_parameters = true,
            dotnet_enable_inlay_hints_for_other_parameters = true,
            dotnet_enable_inlay_hints_for_parameters = true,
            dotnet_suppress_inlay_hints_for_parameters_that_differ_only_by_suffix = true,
            dotnet_suppress_inlay_hints_for_parameters_that_match_argument_name = true,
            dotnet_suppress_inlay_hints_for_parameters_that_match_method_intent = true,
          },
          ["csharp|code_lens"] = {
            dotnet_enable_references_code_lens = true,
          },
          ["csharp|completion"] = {
            dotnet_provide_regex_completions = true,
            dotnet_show_completion_items_from_unimported_namespaces = true,
          },
          ["csharp|formatting"] = {
            dotnet_organize_imports_on_format = true,
          },
        },
      })

      require("roslyn").setup(opts)
      require("csharp").setup()
    end,
  },
}
